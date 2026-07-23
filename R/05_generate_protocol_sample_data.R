############################################################
# 05_generate_protocol_sample_data.R
#
# 改訂研究計画書に準拠した合成データ作成
#
# 出力:
#   data/sample_patient.csv
#   data/sample_landmark_long.csv
#   data/sample_medication_error.csv
############################################################

set.seed(20260722)

dir.create("data", showWarnings = FALSE, recursive = TRUE)
dir.create("results", showWarnings = FALSE, recursive = TRUE)

#-----------------------------------------------------------
# 補助関数
#-----------------------------------------------------------

clamp <- function(x, lower, upper) {
  pmin(pmax(x, lower), upper)
}

round_half <- function(x) {
  round(x * 2) / 2
}

invlogit <- function(x) {
  plogis(x)
}

sample_binary <- function(probability) {
  rbinom(length(probability), 1, probability)
}

#-----------------------------------------------------------
# 原コホート
#-----------------------------------------------------------

n <- 1500

id <- sprintf("P%05d", seq_len(n))

age <- round(
  clamp(
    rnorm(n, mean = 72, sd = 11),
    18,
    98
  )
)

sex <- factor(
  rbinom(n, 1, 0.43),
  levels = c(0, 1),
  labels = c("Male", "Female")
)

stroke_type <- factor(
  sample(
    c("Ischemic", "ICH", "SAH"),
    n,
    replace = TRUE,
    prob = c(0.65, 0.28, 0.07)
  ),
  levels = c("Ischemic", "ICH", "SAH")
)

recurrent_stroke <- factor(
  rbinom(n, 1, 0.24),
  levels = c(0, 1),
  labels = c("First", "Recurrent")
)

onset_to_admission <- round(
  clamp(
    rnorm(n, mean = 34, sd = 14),
    4,
    100
  )
)

calendar_year <- factor(
  sample(
    2022:2026,
    n,
    replace = TRUE
  )
)

premorbid_manager <- factor(
  sample(
    c("Self", "Family", "Care_staff"),
    n,
    replace = TRUE,
    prob = c(0.68, 0.23, 0.09)
  ),
  levels = c("Self", "Family", "Care_staff")
)

#-----------------------------------------------------------
# 入棟時FIM
#-----------------------------------------------------------

fim_motor0 <- round(
  clamp(
    56 -
      0.40 * (age - 72) -
      5 * (stroke_type == "ICH") -
      7 * (stroke_type == "SAH") +
      rnorm(n, 0, 15),
    13,
    91
  )
)

fim_cognitive0 <- round(
  clamp(
    24 -
      0.17 * (age - 72) -
      2 * (stroke_type == "SAH") +
      rnorm(n, 0, 6),
    5,
    35
  )
)

#-----------------------------------------------------------
# 入棟時MRCI-J
#-----------------------------------------------------------

mrci0 <- round_half(
  clamp(
    4 + rgamma(n, shape = 4.5, scale = 3.2),
    2,
    65
  )
)

#-----------------------------------------------------------
# ランドマーク時点
# 行列の列:
# 1 = admission
# 2 = day 30
# 3 = day 60
# 4 = day 90
# 5 = day 120
#-----------------------------------------------------------

snapshot_days <- c(0, 30, 60, 90, 120)
n_snapshot <- length(snapshot_days)

mrci_matrix <- matrix(
  NA_real_,
  nrow = n,
  ncol = n_snapshot
)

fim_motor_matrix <- matrix(
  NA_real_,
  nrow = n,
  ncol = n_snapshot
)

fim_cognitive_matrix <- matrix(
  NA_real_,
  nrow = n,
  ncol = n_snapshot
)

mrci_matrix[, 1] <- mrci0
fim_motor_matrix[, 1] <- fim_motor0
fim_cognitive_matrix[, 1] <- fim_cognitive0

#-----------------------------------------------------------
# MRCI-JおよびFIMの経時変化
#-----------------------------------------------------------

for (k in 2:n_snapshot) {

  latent_simplification <-
    0.08 * (mrci_matrix[, k - 1] - 15) +
    0.015 * (fim_cognitive_matrix[, k - 1] - 20) +
    rnorm(n, mean = 0.8, sd = 2.8)

  delta_mrci <- round_half(
    clamp(
      latent_simplification,
      -8,
      12
    )
  )

  mrci_matrix[, k] <- round_half(
    pmax(
      2,
      mrci_matrix[, k - 1] - delta_mrci
    )
  )

  fim_motor_matrix[, k] <- round(
    clamp(
      fim_motor_matrix[, k - 1] +
        rnorm(n, mean = 6.0 - 0.8 * (k - 2), sd = 5),
      13,
      91
    )
  )

  fim_cognitive_matrix[, k] <- round(
    clamp(
      fim_cognitive_matrix[, k - 1] +
        rnorm(n, mean = 2.3 - 0.3 * (k - 2), sd = 2.5),
      5,
      35
    )
  )
}

#-----------------------------------------------------------
# 家族支援および退院先
#-----------------------------------------------------------

family_support_probability <- invlogit(
  -0.2 +
    1.0 * (premorbid_manager == "Family") +
    0.25 * (age >= 75)
)

family_support <- factor(
  sample_binary(family_support_probability),
  levels = c(0, 1),
  labels = c("No", "Yes")
)

home_probability <- invlogit(
  0.5 +
    0.025 * (fim_motor0 - 50) +
    0.055 * (fim_cognitive0 - 20) +
    0.4 * (family_support == "Yes") -
    0.015 * (age - 72)
)

planned_destination <- ifelse(
  runif(n) < home_probability,
  "Home",
  ifelse(
    runif(n) < 0.70,
    "Facility",
    "Undecided"
  )
)

planned_destination <- factor(
  planned_destination,
  levels = c("Home", "Facility", "Undecided")
)

#-----------------------------------------------------------
# 在院日数・退棟日
#-----------------------------------------------------------

length_of_stay <- round(
  clamp(
    75 +
      0.55 * (91 - fim_motor0) +
      0.85 * (35 - fim_cognitive0) +
      8 * (planned_destination == "Facility") +
      rnorm(n, 0, 22),
    2,
    190
  )
)

#-----------------------------------------------------------
# 患者単位データ
#-----------------------------------------------------------

patient_data <- data.frame(
  id = id,
  age = age,
  sex = sex,
  stroke_type = stroke_type,
  recurrent_stroke = recurrent_stroke,
  onset_to_admission = onset_to_admission,
  calendar_year = calendar_year,
  premorbid_manager = premorbid_manager,
  family_support = family_support,
  planned_destination = planned_destination,
  fim_motor0 = fim_motor0,
  fim_cognitive0 = fim_cognitive0,
  mrci0 = mrci0,
  length_of_stay = length_of_stay
)

#-----------------------------------------------------------
# 完全自己管理開始日の生成
#
# 0～30日の早期自己管理開始を先に生成する。
# その後、30、60、90、120日ランドマークごとに
# 直前期間のMRCI-J変化を用いてイベントを生成する。
#-----------------------------------------------------------

self_management_day <- rep(Inf, n)

early_lp <-
  -0.7 -
  0.018 * (age - 72) -
  0.035 * (mrci0 - 15) +
  0.018 * (fim_motor0 - 50) +
  0.075 * (fim_cognitive0 - 20) +
  0.55 * (premorbid_manager == "Self")

early_rate <- 0.008 * exp(early_lp)

early_candidate <- rexp(n, rate = early_rate)

early_event <- (
  early_candidate <= 30 &
    early_candidate < length_of_stay
)

self_management_day[early_event] <-
  early_candidate[early_event]

#-----------------------------------------------------------
# 逐次ランドマークデータ作成
#-----------------------------------------------------------

landmark_rows <- list()
row_counter <- 0

landmarks <- c(30, 60, 90, 120)

for (j in seq_along(landmarks)) {

  landmark_day <- landmarks[j]

  previous_snapshot_index <- j
  current_snapshot_index <- j + 1

  previous_mrci <- mrci_matrix[, previous_snapshot_index]
  current_mrci <- mrci_matrix[, current_snapshot_index]

  delta_mrci <- round_half(
    previous_mrci - current_mrci
  )

  current_fim_motor <-
    fim_motor_matrix[, current_snapshot_index]

  current_fim_cognitive <-
    fim_cognitive_matrix[, current_snapshot_index]

  # ランドマーク時点で完全自己管理ではなく、
  # かつ在院中である患者
  at_risk <- which(
    self_management_day > landmark_day &
      length_of_stay > landmark_day
  )

  for (i in at_risk) {

    # 直前内服管理レベル
    level_score <-
      0.025 * (current_fim_motor[i] - 50) +
      0.10 * (current_fim_cognitive[i] - 20) -
      0.025 * (current_mrci[i] - 15) +
      rnorm(1, 0, 0.8)

    previous_management_level <- ifelse(
      level_score < -0.4,
      1,
      ifelse(level_score < 0.5, 2, 3)
    )

    # 予定退院までの日数
    days_to_planned_discharge <-
      max(length_of_stay[i] - landmark_day, 0)

    # 医療者判断
    judgement_score <-
      0.025 * (current_fim_motor[i] - 50) +
      0.12 * (current_fim_cognitive[i] - 20) -
      0.030 * (current_mrci[i] - 15) +
      0.55 * (premorbid_manager[i] == "Self") +
      rnorm(1, 0, 0.7)

    clinical_judgement <- ifelse(
      judgement_score < -0.4,
      "Currently_difficult",
      ifelse(
        judgement_score < 0.8,
        "Conditional_or_reassess",
        "Suitable"
      )
    )

    # 完全自己管理移行のハザード
    lp_self <-
      log(1.30) * (delta_mrci[i] / 5) -
      0.015 * (age[i] - 72) +
      0.014 * (current_fim_motor[i] - 50) +
      0.060 * (current_fim_cognitive[i] - 20) +
      0.40 * (premorbid_manager[i] == "Self") +
      0.18 * (family_support[i] == "Yes") +
      0.20 * (planned_destination[i] == "Home") +
      0.20 * (previous_management_level == 3)

    self_rate <- 0.010 * exp(lp_self)
    candidate_self_time <- rexp(1, rate = self_rate)

    competing_time <-
      length_of_stay[i] - landmark_day

    if (
      candidate_self_time <= 30 &&
        candidate_self_time < competing_time
    ) {

      status <- 1
      followup_days <- candidate_self_time
      self_management_day[i] <-
        landmark_day + candidate_self_time
      competing_type <- NA_character_

    } else if (competing_time <= 30) {

      status <- 2
      followup_days <- competing_time

      competing_type <- sample(
        c(
          "Discharge_without_self_management",
          "Transfer",
          "Death",
          "All_target_medications_stopped"
        ),
        size = 1,
        prob = c(0.88, 0.07, 0.01, 0.04)
      )

    } else {

      status <- 0
      followup_days <- 30
      competing_type <- NA_character_
    }

    row_counter <- row_counter + 1

    landmark_rows[[row_counter]] <- data.frame(
      id = id[i],
      landmark = landmark_day,
      previous_mrci = previous_mrci[i],
      current_mrci = current_mrci[i],
      delta_mrci = delta_mrci[i],
      delta5 = delta_mrci[i] / 5,
      age = age[i],
      sex = sex[i],
      stroke_type = stroke_type[i],
      recurrent_stroke = recurrent_stroke[i],
      onset_to_admission = onset_to_admission[i],
      calendar_year = calendar_year[i],
      fim_motor = current_fim_motor[i],
      fim_cognitive = current_fim_cognitive[i],
      previous_management_level =
        previous_management_level,
      premorbid_manager = premorbid_manager[i],
      family_support = family_support[i],
      planned_destination =
        planned_destination[i],
      days_to_planned_discharge =
        days_to_planned_discharge,
      clinical_judgement = clinical_judgement,
      self_management_30d = as.integer(status == 1),
      followup_days = followup_days,
      status = status,
      competing_type = competing_type
    )
  }
}

landmark_data <- do.call(
  rbind,
  landmark_rows
)

landmark_data$landmark <- factor(
  landmark_data$landmark,
  levels = c(30, 60, 90, 120)
)

landmark_data$previous_management_level <- factor(
  landmark_data$previous_management_level,
  levels = c(1, 2, 3),
  ordered = TRUE
)

landmark_data$clinical_judgement <- factor(
  landmark_data$clinical_judgement,
  levels = c(
    "Currently_difficult",
    "Conditional_or_reassess",
    "Suitable"
  )
)

#-----------------------------------------------------------
# 完全自己管理開始後の服薬エラー解析データ
#-----------------------------------------------------------

self_indices <- which(
  is.finite(self_management_day) &
    self_management_day < length_of_stay
)

error_rows <- vector(
  "list",
  length(self_indices)
)

for (z in seq_along(self_indices)) {

  i <- self_indices[z]
  start_day <- self_management_day[i]

  current_snapshot_index <- max(
    which(snapshot_days <= start_day)
  )

  current_snapshot_index <- min(
    current_snapshot_index,
    n_snapshot
  )

  previous_snapshot_index <- max(
    1,
    current_snapshot_index - 1
  )

  mrci_start <- mrci_matrix[
    i,
    current_snapshot_index
  ]

  mrci_previous <- mrci_matrix[
    i,
    previous_snapshot_index
  ]

  delta_mrci_pre30 <-
    mrci_previous - mrci_start

  medication_count <- round(
    clamp(
      2 + 0.28 * mrci_start + rnorm(1, 0, 1.8),
      1,
      18
    )
  )

  dosing_times <- round(
    clamp(
      1 + 0.10 * mrci_start + rnorm(1, 0, 0.8),
      1,
      6
    )
  )

  daily_opportunities <- max(
    medication_count,
    round(
      medication_count *
        runif(1, 1.0, 2.0)
    )
  )

  irregular_regimen <- rbinom(
    1,
    1,
    invlogit(-2 + 0.06 * mrci_start)
  )

  multiple_prescriptions <- rbinom(
    1,
    1,
    invlogit(-1.3 + 0.04 * mrci_start)
  )

  one_dose_package <- rbinom(
    1,
    1,
    invlogit(0.3 + 0.02 * mrci_start)
  )

  mixed_package <- rbinom(
    1,
    1,
    invlogit(-1.5 + 0.04 * mrci_start)
  )

  brought_in_and_hospital_mix <- rbinom(
    1,
    1,
    0.15
  )

  prescription_changes_pre30 <- rpois(
    1,
    lambda = clamp(
      0.8 + abs(delta_mrci_pre30) / 3,
      0.1,
      6
    )
  )

  days_since_last_change <- round(
    clamp(
      rexp(
        1,
        rate = 1 / 10
      ),
      0,
      30
    )
  )

  # 自己管理終了、退棟等までの時間
  remaining_stay <-
    max(length_of_stay[i] - start_day, 0.1)

  management_stop_time <- rexp(
    1,
    rate = 0.015
  )

  count_followup_days <- min(
    30,
    remaining_stay,
    management_stop_time
  )

  # 1日当たり服薬エラー率
  error_lp <-
    log(1.18) * (mrci_start / 5) -
    log(1.15) * (delta_mrci_pre30 / 5) +
    0.35 * irregular_regimen +
    0.25 * multiple_prescriptions +
    0.30 * mixed_package +
    0.20 * brought_in_and_hospital_mix -
    0.20 * one_dose_package +
    0.40 * (days_since_last_change <= 2) +
    0.20 * (days_since_last_change >= 3 &
              days_since_last_change <= 7) -
    0.018 * (fim_cognitive_matrix[
      i,
      current_snapshot_index
    ] - 20)

  daily_error_rate <- 0.007 * exp(error_lp)

  error_count <- rpois(
    1,
    lambda = daily_error_rate *
      count_followup_days
  )

  if (error_count > 0) {

    event_times <- runif(
      error_count,
      min = 0.05,
      max = max(
        count_followup_days,
        0.06
      )
    )

    first_error_time <- min(event_times)
    first_error_status <- 1

  } else {

    first_error_time <- count_followup_days

    first_error_status <- ifelse(
      count_followup_days < 30,
      2,
      0
    )
  }

  medication_opportunities <- round(
    daily_opportunities *
      count_followup_days
  )

  error_rows[[z]] <- data.frame(
    id = id[i],
    self_management_start_day = start_day,
    mrci_start = mrci_start,
    mrci_start_5 = mrci_start / 5,
    mrci_previous = mrci_previous,
    delta_mrci_pre30 = delta_mrci_pre30,
    delta_mrci_pre30_5 =
      delta_mrci_pre30 / 5,
    medication_count = medication_count,
    dosing_times = dosing_times,
    daily_opportunities = daily_opportunities,
    irregular_regimen = irregular_regimen,
    multiple_prescriptions =
      multiple_prescriptions,
    one_dose_package = one_dose_package,
    mixed_package = mixed_package,
    brought_in_and_hospital_mix =
      brought_in_and_hospital_mix,
    prescription_changes_pre30 =
      prescription_changes_pre30,
    days_since_last_change =
      days_since_last_change,
    age = age[i],
    sex = sex[i],
    stroke_type = stroke_type[i],
    fim_motor =
      fim_motor_matrix[i, current_snapshot_index],
    fim_cognitive =
      fim_cognitive_matrix[
        i,
        current_snapshot_index
      ],
    premorbid_manager =
      premorbid_manager[i],
    first_error_time =
      first_error_time,
    first_error_status =
      first_error_status,
    error_30d =
      as.integer(error_count > 0),
    error_count = error_count,
    self_management_patient_days =
      count_followup_days,
    medication_opportunities =
      medication_opportunities
  )
}

error_data <- do.call(
  rbind,
  error_rows
)

#-----------------------------------------------------------
# データ品質確認
#-----------------------------------------------------------

stopifnot(
  all(
    abs(
      landmark_data$delta_mrci -
        (
          landmark_data$previous_mrci -
            landmark_data$current_mrci
        )
    ) < 1e-8
  )
)

stopifnot(
  all(
    landmark_data$status %in% c(0, 1, 2)
  )
)

stopifnot(
  all(
    landmark_data$followup_days > 0 &
      landmark_data$followup_days <= 30
  )
)

stopifnot(
  all(
    error_data$first_error_status %in%
      c(0, 1, 2)
  )
)

stopifnot(
  all(
    error_data$self_management_patient_days > 0
  )
)

#-----------------------------------------------------------
# 保存
#-----------------------------------------------------------

write.csv(
  patient_data,
  "data/sample_patient.csv",
  row.names = FALSE,
  na = ""
)

write.csv(
  landmark_data,
  "data/sample_landmark_long.csv",
  row.names = FALSE,
  na = ""
)

write.csv(
  error_data,
  "data/sample_medication_error.csv",
  row.names = FALSE,
  na = ""
)

saveRDS(
  patient_data,
  "data/sample_patient.rds"
)

saveRDS(
  landmark_data,
  "data/sample_landmark_long.rds"
)

saveRDS(
  error_data,
  "data/sample_medication_error.rds"
)

cat("\n合成データ作成完了\n")
cat("原コホート:", nrow(patient_data), "\n")
cat("ランドマーク行数:", nrow(landmark_data), "\n")
cat(
  "完全自己管理移行数:",
  sum(landmark_data$status == 1),
  "\n"
)
cat("服薬エラー解析対象:", nrow(error_data), "\n")
cat(
  "服薬エラー経験者:",
  sum(error_data$error_30d),
  "\n"
)

print(
  with(
    landmark_data,
    table(landmark, status)
  )
)
