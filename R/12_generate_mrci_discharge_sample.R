############################################################
# 12_generate_mrci_discharge_sample.R
#
# 入院時MRCI-Jと退院時完全内服自己管理の研究用
# 合成サンプルデータ
############################################################

options(stringsAsFactors = FALSE, warn = 1)

if (requireNamespace("rprojroot", quietly = TRUE)) {
  ROOT <- rprojroot::find_root(
    rprojroot::is_git_root |
      rprojroot::has_file("research.Rproj")
  )
} else {
  ROOT <- getwd()
}

DATA_DIR <- file.path(ROOT, "data")
dir.create(DATA_DIR, recursive = TRUE, showWarnings = FALSE)

set.seed(20260724)

clamp <- function(x, lower, upper) {
  pmin(pmax(x, lower), upper)
}

round_half <- function(x) {
  round(x * 2) / 2
}

n <- 500

#-----------------------------------------------------------
# 患者背景
#-----------------------------------------------------------

d <- data.frame(
  id = sprintf("P%04d", seq_len(n)),

  age = round(clamp(rnorm(n, 73, 11), 18, 98)),

  sex = factor(
    rbinom(n, 1, 0.43),
    levels = c(0, 1),
    labels = c("Male", "Female")
  ),

  stroke_type = factor(
    sample(
      c("Ischemic", "ICH", "SAH"),
      n,
      replace = TRUE,
      prob = c(0.67, 0.27, 0.06)
    ),
    levels = c("Ischemic", "ICH", "SAH")
  ),

  first_recurrent = factor(
    rbinom(n, 1, 0.24),
    levels = c(0, 1),
    labels = c("First", "Recurrent")
  ),

  ward = factor(
    sample(
      c("Rehabilitation", "Acute"),
      n,
      replace = TRUE,
      prob = c(0.85, 0.15)
    ),
    levels = c("Rehabilitation", "Acute")
  ),

  admission_year = factor(
    sample(2022:2026, n, replace = TRUE)
  ),

  premorbid_manager = factor(
    sample(
      c("Self", "Family", "Care_staff"),
      n,
      replace = TRUE,
      prob = c(0.67, 0.25, 0.08)
    ),
    levels = c("Self", "Family", "Care_staff")
  ),

  premorbid_mrs = ordered(
    sample(
      0:5,
      n,
      replace = TRUE,
      prob = c(0.25, 0.25, 0.20, 0.16, 0.10, 0.04)
    ),
    levels = 0:5
  ),

  onset_to_ward_days = round(
    clamp(rnorm(n, 28, 17), 0, 120)
  ),

  family_support0 = factor(
    rbinom(n, 1, 0.62),
    levels = c(0, 1),
    labels = c("No", "Yes")
  ),

  comorbidity_index = clamp(
    rpois(n, lambda = 2.1),
    0,
    9
  )
)

#-----------------------------------------------------------
# 入院時機能
#-----------------------------------------------------------

d$fim_motor0 <- round(
  clamp(
    58 -
      0.45 * (d$age - 73) -
      5 * (d$stroke_type == "ICH") -
      7 * (d$stroke_type == "SAH") +
      rnorm(n, 0, 15),
    13,
    91
  )
)

d$fim_cognitive0 <- round(
  clamp(
    24 -
      0.18 * (d$age - 73) -
      2 * (d$stroke_type == "SAH") +
      rnorm(n, 0, 6),
    5,
    35
  )
)

d$nihss0 <- round(
  clamp(
    16 -
      0.12 * d$fim_motor0 -
      0.18 * d$fim_cognitive0 +
      rnorm(n, 6, 4),
    0,
    35
  )
)

d$aphasia <- rbinom(
  n, 1,
  plogis(-1.3 - 0.09 * (d$fim_cognitive0 - 20))
)

d$neglect <- rbinom(
  n, 1,
  plogis(-1.7 - 0.055 * (d$fim_motor0 - 50))
)

d$upper_limb_paresis <- rbinom(
  n, 1,
  plogis(-0.5 - 0.045 * (d$fim_motor0 - 50))
)

#-----------------------------------------------------------
# 入院時MRCI-J
# Section A + B + C = 総得点
#-----------------------------------------------------------

d$mrci_a0 <- round_half(
  clamp(
    1 + rgamma(n, shape = 1.7, scale = 0.8),
    1,
    6
  )
)

d$mrci_b0 <- round_half(
  clamp(
    3 + rgamma(n, shape = 4.5, scale = 1.8),
    2,
    28
  )
)

d$mrci_c0 <- round_half(
  clamp(
    rgamma(n, shape = 2.2, scale = 1.6),
    0,
    16
  )
)

d$mrci0 <- round_half(
  d$mrci_a0 + d$mrci_b0 + d$mrci_c0
)

d$medication_count0 <- pmax(
  1,
  round(1 + d$mrci_b0 / 2.3 + rnorm(n, 0, 1.2))
)

d$dosing_times0 <- clamp(
  round(1 + d$mrci_b0 / 6 + rnorm(n, 0, 0.6)),
  1,
  5
)

d$daily_doses0 <- pmax(
  d$dosing_times0,
  round(d$medication_count0 * runif(n, 1.0, 1.8))
)

d$mrci_prn0 <- round_half(
  d$mrci0 + rbinom(n, 1, 0.35) * runif(n, 0.5, 3)
)

d$mrci_all0 <- round_half(
  d$mrci_prn0 + rbinom(n, 1, 0.30) * runif(n, 0.5, 5)
)

#-----------------------------------------------------------
# 入院時完全自己管理
#-----------------------------------------------------------

p_initial_self <- plogis(
  -0.7 -
    0.03 * (d$age - 73) +
    0.08 * (d$fim_cognitive0 - 24) +
    0.015 * (d$fim_motor0 - 58) -
    0.035 * (d$mrci0 - 15) +
    1.0 * (d$premorbid_manager == "Self")
)

d$initial_complete_self <- rbinom(n, 1, p_initial_self)

#-----------------------------------------------------------
# 自己管理試行
#-----------------------------------------------------------

p_trial <- plogis(
  0.2 -
    0.035 * (d$age - 73) +
    0.075 * (d$fim_cognitive0 - 24) +
    0.020 * (d$fim_motor0 - 58) -
    0.045 * (d$mrci0 - 15) +
    0.8 * (d$premorbid_manager == "Self")
)

d$trial_started <- ifelse(
  d$initial_complete_self == 1,
  1,
  rbinom(n, 1, p_trial)
)

trial_reasons <- c(
  "Functional_or_cognitive",
  "Safety",
  "Unstable_condition",
  "Patient_refusal",
  "Family_preference",
  "Not_required_after_discharge",
  "Sudden_discharge",
  "Ward_operation",
  "Clinician_decision",
  "Unknown"
)

d$no_trial_reason <- NA_character_
d$no_trial_reason[d$trial_started == 0] <- sample(
  trial_reasons,
  sum(d$trial_started == 0),
  replace = TRUE
)

#-----------------------------------------------------------
# 退院時完全自己管理
#-----------------------------------------------------------

lp_outcome <-
  -0.2 -
  0.025 * (d$age - 73) -
  0.055 * ((d$mrci0 - 15) / 5) +
  0.018 * (d$fim_motor0 - 58) +
  0.095 * (d$fim_cognitive0 - 24) +
  0.70 * (d$premorbid_manager == "Self") -
  0.45 * (d$premorbid_manager == "Care_staff") +
  0.25 * (d$family_support0 == "Yes") -
  0.20 * as.numeric(d$premorbid_mrs) +
  1.1 * d$initial_complete_self

d$discharge_complete_self <- rbinom(
  n,
  1,
  plogis(lp_outcome)
)

d$discharge_management_level <- ifelse(
  d$discharge_complete_self == 1,
  4,
  sample(1:3, n, replace = TRUE, prob = c(0.45, 0.35, 0.20))
)

#-----------------------------------------------------------
# 退院前MRCI-J
# Δ = 退院前 - 入院時
#-----------------------------------------------------------

d$mrci_discharge <- round_half(
  pmax(
    1,
    d$mrci0 +
      rnorm(n, mean = -1.2, sd = 3.0) -
      1.2 * d$trial_started
  )
)

d$delta_mrci <- round_half(
  d$mrci_discharge - d$mrci0
)

#-----------------------------------------------------------
# 服薬エラー
#-----------------------------------------------------------

d$self_management_patient_days <- ifelse(
  d$trial_started == 1,
  sample(5:70, n, replace = TRUE),
  0
)

error_rate <- exp(
  -4.3 +
    0.045 * (d$mrci0 - 15) -
    0.035 * (d$fim_cognitive0 - 24)
)

d$medication_error_count <- ifelse(
  d$trial_started == 1,
  rpois(n, error_rate * d$self_management_patient_days),
  NA_integer_
)

d$medication_error_any <- ifelse(
  d$trial_started == 1,
  as.integer(d$medication_error_count > 0),
  NA_integer_
)

#-----------------------------------------------------------
# 感度分析用変数
#-----------------------------------------------------------

d$home_discharge <- rbinom(
  n, 1,
  plogis(
    0.8 +
      0.02 * (d$fim_motor0 - 58) +
      0.04 * (d$fim_cognitive0 - 24)
  )
)

d$dementia <- rbinom(
  n, 1,
  plogis(-2 + 0.055 * (d$age - 73))
)

d$prescription_confirmed_72h <- rbinom(n, 1, 0.92)
d$discharge_rx_consistent <- rbinom(n, 1, 0.90)
d$trial_opportunity_unavailable <- as.integer(
  d$no_trial_reason %in% c("Sudden_discharge", "Ward_operation")
)

#-----------------------------------------------------------
# 共変量に欠測を付与
# 主要曝露・アウトカムは欠測にしない
#-----------------------------------------------------------

d$fim_cognitive0[
  sample(seq_len(n), round(0.06 * n))
] <- NA

d$family_support0[
  sample(seq_len(n), round(0.08 * n))
] <- NA

d$comorbidity_index[
  sample(seq_len(n), round(0.05 * n))
] <- NA

#-----------------------------------------------------------
# 保存
#-----------------------------------------------------------

write.csv(
  d,
  file.path(DATA_DIR, "sample_mrci_discharge.csv"),
  row.names = FALSE,
  na = ""
)

saveRDS(
  d,
  file.path(DATA_DIR, "sample_mrci_discharge.rds")
)

cat(
  "\nサンプルデータ作成完了:",
  nrow(d),
  "例\n"
)
