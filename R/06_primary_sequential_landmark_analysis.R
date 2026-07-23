############################################################
# 06_primary_sequential_landmark_analysis.R
#
# 主要解析:
#   30、60、90、120日逐次ランドマーク
#   pooled logistic regression
#   患者単位cluster-robust SE
#   標準化30日移行確率
#   MRCI-J 5点低下に対応する絶対リスク差
#
# 副次解析:
#   cause-specific Cox
#   Aalen-Johansen累積発生関数
#   非線形性
#   ランドマーク時期との交互作用
############################################################

#============================================================
# 共通パス設定
#============================================================

R_DIRECTORY <- normalizePath(
  "/Users/nakamuratatsuya/Desktop/R",
  mustWork = TRUE
)

DATA_DIRECTORY <- file.path(
  R_DIRECTORY,
  "data"
)

RESULTS_DIRECTORY <- file.path(
  R_DIRECTORY,
  "results"
)

FIGURE_DIRECTORY <- file.path(
  RESULTS_DIRECTORY,
  "figures"
)

dir.create(
  DATA_DIRECTORY,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  RESULTS_DIRECTORY,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  FIGURE_DIRECTORY,
  recursive = TRUE,
  showWarnings = FALSE
)

setwd(R_DIRECTORY)

cat(
  "\nR directory:",
  R_DIRECTORY,
  "\nData directory:",
  DATA_DIRECTORY,
  "\nResults directory:",
  RESULTS_DIRECTORY,
  "\n"
)

required_packages <- c(
  "survival",
  "sandwich",
  "lmtest",
  "splines",
  "cmprsk",
  "ggplot2"
)

not_installed <- required_packages[
  !required_packages %in%
    rownames(installed.packages())
]

if (length(not_installed) > 0) {
  install.packages(not_installed)
}

library(survival)
library(sandwich)
library(lmtest)
library(splines)
library(cmprsk)
library(ggplot2)

#-----------------------------------------------------------
# データ読み込み
#-----------------------------------------------------------

landmark_data_file <- file.path(
  DATA_DIRECTORY,
  "sample_landmark_long.rds"
)

if (!file.exists(landmark_data_file)) {
  stop(
    paste0(
      "ランドマークデータがありません:\n",
      landmark_data_file,
      "\n\n",
      "先に05_generate_protocol_sample_data.Rを実行してください。"
    )
  )
}

d <- readRDS(
  landmark_data_file
)

d <- d[
  order(d$id, as.numeric(as.character(d$landmark))),
]

#-----------------------------------------------------------
# 変数型
#-----------------------------------------------------------

d$id <- factor(d$id)

d$landmark <- factor(
  d$landmark,
  levels = c(30, 60, 90, 120)
)

d$sex <- factor(
  d$sex,
  levels = c("Male", "Female")
)

d$stroke_type <- factor(
  d$stroke_type,
  levels = c("Ischemic", "ICH", "SAH")
)

d$recurrent_stroke <- factor(
  d$recurrent_stroke,
  levels = c("First", "Recurrent")
)

d$calendar_year <- factor(d$calendar_year)

d$premorbid_manager <- factor(
  d$premorbid_manager,
  levels = c("Self", "Family", "Care_staff")
)

d$family_support <- factor(
  d$family_support,
  levels = c("No", "Yes")
)

d$planned_destination <- factor(
  d$planned_destination,
  levels = c("Home", "Facility", "Undecided")
)

d$previous_management_level <- factor(
  d$previous_management_level,
  levels = c(1, 2, 3),
  ordered = FALSE
)

#-----------------------------------------------------------
# 品質確認
#-----------------------------------------------------------

stopifnot(
  all(d$status %in% c(0, 1, 2))
)

stopifnot(
  all(
    d$self_management_30d ==
      as.integer(d$status == 1)
  )
)

stopifnot(
  all(
    abs(
      d$delta_mrci -
        (d$previous_mrci - d$current_mrci)
    ) < 1e-8
  )
)

#-----------------------------------------------------------
# 記述統計
#-----------------------------------------------------------

landmark_flow <- do.call(
  rbind,
  lapply(
    split(d, d$landmark),
    function(x) {
      data.frame(
        landmark = unique(x$landmark),
        risk_set_n = nrow(x),
        unique_patients = length(unique(x$id)),
        self_management = sum(x$status == 1),
        competing_event = sum(x$status == 2),
        no_event_30d = sum(x$status == 0),
        mean_mrci = mean(x$current_mrci),
        mean_delta_mrci = mean(x$delta_mrci)
      )
    }
  )
)

write.csv(
  landmark_flow,
  file.path(
    RESULTS_DIRECTORY,
    "landmark_flow.csv"
  ),
  row.names = FALSE
)

print(landmark_flow)

#-----------------------------------------------------------
# 主要モデル
#
# 競合イベントが先行した場合:
# self_management_30d = 0
#
# 同一患者の複数ランドマーク参加:
# 患者単位cluster-robust variance
#-----------------------------------------------------------

main_formula <- self_management_30d ~
  delta5 +
  age +
  sex +
  stroke_type +
  recurrent_stroke +
  onset_to_admission +
  previous_mrci +
  fim_motor +
  fim_cognitive +
  previous_management_level +
  premorbid_manager +
  family_support +
  planned_destination +
  landmark +
  calendar_year

fit_main <- glm(
  main_formula,
  data = d,
  family = binomial(link = "logit"),
  na.action = na.exclude,
  x = TRUE,
  y = TRUE
)

main_vcov <- vcovCL(
  fit_main,
  cluster = d$id,
  type = "HC0"
)

main_coeftest <- coeftest(
  fit_main,
  vcov. = main_vcov
)

main_results <- data.frame(
  variable = rownames(main_coeftest),
  coefficient = main_coeftest[, 1],
  robust_se = main_coeftest[, 2],
  odds_ratio = exp(main_coeftest[, 1]),
  ci_lower = exp(
    main_coeftest[, 1] -
      1.96 * main_coeftest[, 2]
  ),
  ci_upper = exp(
    main_coeftest[, 1] +
      1.96 * main_coeftest[, 2]
  ),
  p_value = main_coeftest[, 4],
  row.names = NULL
)

write.csv(
  main_results,
  file.path(
    RESULTS_DIRECTORY,
    "main_pooled_logistic.csv"
  ),
  row.names = FALSE
)

print(
  subset(
    main_results,
    variable == "delta5"
  )
)

#-----------------------------------------------------------
# 標準化予測関数
#
# 観察された各患者・ランドマーク行について、
# delta_mrciを一律5点増加させた場合と
# 観察値の場合を比較する。
#
# これは因果効果ではなく、
# fitted modelに基づく標準化関連である。
#-----------------------------------------------------------

standardized_shift <- function(
  fit,
  data,
  shift_delta5 = 1
) {

  observed_data <- data
  shifted_data <- data

  shifted_data$delta5 <-
    shifted_data$delta5 + shift_delta5

  p_observed <- predict(
    fit,
    newdata = observed_data,
    type = "response"
  )

  p_shifted <- predict(
    fit,
    newdata = shifted_data,
    type = "response"
  )

  risk0 <- mean(p_observed, na.rm = TRUE)
  risk1 <- mean(p_shifted, na.rm = TRUE)

  data.frame(
    observed_standardized_risk = risk0,
    shifted_standardized_risk = risk1,
    risk_difference = risk1 - risk0,
    risk_ratio = risk1 / risk0
  )
}

standardized_point <- standardized_shift(
  fit_main,
  d,
  shift_delta5 = 1
)

print(standardized_point)

#-----------------------------------------------------------
# 患者単位cluster bootstrap
#
# 同一患者の全ランドマーク行をまとめて再抽出する。
#-----------------------------------------------------------

cluster_bootstrap_standardized <- function(
  data,
  formula,
  repetitions = 500,
  seed = 20260722
) {

  set.seed(seed)

  unique_ids <- unique(
    as.character(data$id)
  )

  bootstrap_results <- matrix(
    NA_real_,
    nrow = repetitions,
    ncol = 4
  )

  colnames(bootstrap_results) <- c(
    "risk_observed",
    "risk_shifted",
    "risk_difference",
    "risk_ratio"
  )

  for (b in seq_len(repetitions)) {

    sampled_ids <- sample(
      unique_ids,
      size = length(unique_ids),
      replace = TRUE
    )

    bootstrap_parts <- vector(
      "list",
      length(sampled_ids)
    )

    for (k in seq_along(sampled_ids)) {

      tmp <- data[
        as.character(data$id) ==
          sampled_ids[k],
        ,
        drop = FALSE
      ]

      tmp$id <- factor(
        paste0("B", b, "_", k)
      )

      bootstrap_parts[[k]] <- tmp
    }

    db <- do.call(
      rbind,
      bootstrap_parts
    )

    fit_b <- try(
      glm(
        formula,
        data = db,
        family = binomial(),
        na.action = na.exclude
      ),
      silent = TRUE
    )

    if (inherits(fit_b, "try-error")) {
      next
    }

    estimate_b <- standardized_shift(
      fit_b,
      db,
      shift_delta5 = 1
    )

    bootstrap_results[b, ] <- unlist(
      estimate_b[1, ]
    )
  }

  bootstrap_results <- as.data.frame(
    bootstrap_results
  )

  bootstrap_results <- bootstrap_results[
    complete.cases(bootstrap_results),
    ,
    drop = FALSE
  ]

  list(
    bootstrap_estimates = bootstrap_results,
    confidence_intervals = data.frame(
      estimand = names(bootstrap_results),
      lower = vapply(
        bootstrap_results,
        quantile,
        numeric(1),
        probs = 0.025,
        na.rm = TRUE
      ),
      upper = vapply(
        bootstrap_results,
        quantile,
        numeric(1),
        probs = 0.975,
        na.rm = TRUE
      )
    )
  )
}

bootstrap_result <- cluster_bootstrap_standardized(
  data = d,
  formula = main_formula,
  repetitions = 500
)

standardized_result <- data.frame(
  estimand = c(
    "risk_observed",
    "risk_shifted",
    "risk_difference",
    "risk_ratio"
  ),
  estimate = unlist(standardized_point[1, ]),
  lower = bootstrap_result$
    confidence_intervals$lower,
  upper = bootstrap_result$
    confidence_intervals$upper
)

write.csv(
  standardized_result,
  file.path(
    RESULTS_DIRECTORY,
    "main_standardized_risk.csv"
  ),
  row.names = FALSE
)

write.csv(
  bootstrap_result$bootstrap_estimates,
  file.path(
    RESULTS_DIRECTORY,
    "main_standardized_bootstrap.csv"
  ),
  row.names = FALSE
)

print(standardized_result)

#-----------------------------------------------------------
# モデル1：基本調整モデル
#-----------------------------------------------------------

fit_model1 <- glm(
  self_management_30d ~
    delta5 +
    age +
    sex +
    stroke_type +
    onset_to_admission +
    previous_mrci +
    landmark +
    calendar_year,
  data = d,
  family = binomial()
)

#-----------------------------------------------------------
# モデル2：患者状態調整モデル
#-----------------------------------------------------------

fit_model2 <- fit_main

#-----------------------------------------------------------
# モデル3：医療者判断・退院計画追加
#-----------------------------------------------------------

fit_model3 <- glm(
  update(
    main_formula,
    . ~ . +
      clinical_judgement +
      days_to_planned_discharge
  ),
  data = d,
  family = binomial()
)

extract_robust_glm <- function(fit, cluster) {

  V <- vcovCL(
    fit,
    cluster = cluster,
    type = "HC0"
  )

  result <- coeftest(
    fit,
    vcov. = V
  )

  data.frame(
    variable = rownames(result),
    estimate = result[, 1],
    robust_se = result[, 2],
    odds_ratio = exp(result[, 1]),
    ci_lower = exp(
      result[, 1] - 1.96 * result[, 2]
    ),
    ci_upper = exp(
      result[, 1] + 1.96 * result[, 2]
    ),
    p_value = result[, 4],
    row.names = NULL
  )
}

model1_results <- extract_robust_glm(
  fit_model1,
  d$id
)

model2_results <- extract_robust_glm(
  fit_model2,
  d$id
)

model3_results <- extract_robust_glm(
  fit_model3,
  d$id
)

write.csv(
  model1_results,
  file.path(
    RESULTS_DIRECTORY,
    "model1_basic.csv"
  ),
  row.names = FALSE
)

write.csv(
  model2_results,
  file.path(
    RESULTS_DIRECTORY,
    "model2_patient_state.csv"
  ),
  row.names = FALSE
)

write.csv(
  model3_results,
  file.path(
    RESULTS_DIRECTORY,
    "model3_clinical_judgement.csv"
  ),
  row.names = FALSE
)

#-----------------------------------------------------------
# 非線形性：restricted/natural cubic spline
#-----------------------------------------------------------

fit_spline <- glm(
  update(
    main_formula,
    . ~ . - delta5 +
      ns(delta5, df = 3)
  ),
  data = d,
  family = binomial()
)

nonlinearity_test <- anova(
  fit_main,
  fit_spline,
  test = "LRT"
)

capture.output(
  nonlinearity_test,
  file = file.path(
    RESULTS_DIRECTORY,
    "nonlinearity_test.txt"
  )
)

#-----------------------------------------------------------
# ランドマーク時期との交互作用
# 90日と120日を統合
#-----------------------------------------------------------

d$landmark_group <- factor(
  ifelse(
    d$landmark == "30",
    "Day30",
    ifelse(
      d$landmark == "60",
      "Day60",
      "Day90_120"
    )
  ),
  levels = c(
    "Day30",
    "Day60",
    "Day90_120"
  )
)

fit_interaction <- glm(
  self_management_30d ~
    delta5 * landmark_group +
    age +
    sex +
    stroke_type +
    recurrent_stroke +
    onset_to_admission +
    previous_mrci +
    fim_motor +
    fim_cognitive +
    previous_management_level +
    premorbid_manager +
    family_support +
    planned_destination +
    calendar_year,
  data = d,
  family = binomial()
)

interaction_results <- extract_robust_glm(
  fit_interaction,
  d$id
)

write.csv(
  interaction_results,
  file.path(
    RESULTS_DIRECTORY,
    "landmark_interaction.csv"
  ),
  row.names = FALSE
)

#-----------------------------------------------------------
# cause-specific Cox回帰
#
# 競合イベントは打切りとして扱う。
# HRは累積移行確率比ではない。
#-----------------------------------------------------------

fit_cs_cox <- coxph(
  Surv(
    followup_days,
    status == 1
  ) ~
    delta5 +
    age +
    sex +
    stroke_type +
    recurrent_stroke +
    onset_to_admission +
    previous_mrci +
    fim_motor +
    fim_cognitive +
    previous_management_level +
    premorbid_manager +
    family_support +
    planned_destination +
    landmark +
    calendar_year +
    cluster(id),
  data = d,
  ties = "efron",
  x = TRUE,
  y = TRUE
)

cox_summary <- summary(fit_cs_cox)

cox_results <- data.frame(
  variable = rownames(
    cox_summary$coefficients
  ),
  hazard_ratio =
    cox_summary$conf.int[, "exp(coef)"],
  ci_lower =
    cox_summary$conf.int[, "lower .95"],
  ci_upper =
    cox_summary$conf.int[, "upper .95"],
  robust_se =
    cox_summary$coefficients[, "robust se"],
  p_value =
    cox_summary$coefficients[, "Pr(>|z|)"],
  row.names = NULL
)

write.csv(
  cox_results,
  file.path(
    RESULTS_DIRECTORY,
    "cause_specific_cox.csv"
  ),
  row.names = FALSE
)

#-----------------------------------------------------------
# Aalen-Johansen / cumulative incidence
# ランドマーク別
#-----------------------------------------------------------

cif_results <- list()

for (lm_value in levels(d$landmark)) {

  dlm <- subset(
    d,
    landmark == lm_value
  )

  cif_results[[lm_value]] <- cuminc(
    ftime = dlm$followup_days,
    fstatus = dlm$status,
    cencode = 0
  )
}

saveRDS(
  cif_results,
  file.path(
    RESULTS_DIRECTORY,
    "cumulative_incidence_by_landmark.rds"
  )
)

#-----------------------------------------------------------
# 現在MRCI-Jモデル
#-----------------------------------------------------------

fit_current_mrci <- glm(
  update(
    main_formula,
    . ~ . - delta5 +
      current_mrci
  ),
  data = d,
  family = binomial()
)

current_mrci_results <- extract_robust_glm(
  fit_current_mrci,
  d$id
)

write.csv(
  current_mrci_results,
  file.path(
    RESULTS_DIRECTORY,
    "current_mrci_model.csv"
  ),
  row.names = FALSE
)

#-----------------------------------------------------------
# 直前MRCI-J＋現在MRCI-Jモデル
#-----------------------------------------------------------

fit_previous_current <- glm(
  self_management_30d ~
    previous_mrci +
    current_mrci +
    age +
    sex +
    stroke_type +
    recurrent_stroke +
    onset_to_admission +
    fim_motor +
    fim_cognitive +
    previous_management_level +
    premorbid_manager +
    family_support +
    planned_destination +
    landmark +
    calendar_year,
  data = d,
  family = binomial()
)

previous_current_results <- extract_robust_glm(
  fit_previous_current,
  d$id
)

write.csv(
  previous_current_results,
  file.path(
    RESULTS_DIRECTORY,
    "previous_current_mrci_model.csv"
  ),
  row.names = FALSE
)

#-----------------------------------------------------------
# 30日ランドマーク限定感度分析
#-----------------------------------------------------------

d30 <- subset(
  d,
  landmark == "30"
)

fit_day30 <- glm(
  self_management_30d ~
    delta5 +
    age +
    sex +
    stroke_type +
    recurrent_stroke +
    onset_to_admission +
    previous_mrci +
    fim_motor +
    fim_cognitive +
    previous_management_level +
    premorbid_manager +
    family_support +
    planned_destination +
    calendar_year,
  data = d30,
  family = binomial()
)

day30_results <- extract_robust_glm(
  fit_day30,
  d30$id
)

write.csv(
  day30_results,
  file.path(
    RESULTS_DIRECTORY,
    "sensitivity_day30_only.csv"
  ),
  row.names = FALSE
)

#-----------------------------------------------------------
# 保存
#-----------------------------------------------------------

saveRDS(
  list(
    fit_main = fit_main,
    main_vcov = main_vcov,
    fit_model1 = fit_model1,
    fit_model2 = fit_model2,
    fit_model3 = fit_model3,
    fit_spline = fit_spline,
    fit_interaction = fit_interaction,
    fit_cs_cox = fit_cs_cox,
    fit_current_mrci = fit_current_mrci,
    fit_previous_current = fit_previous_current,
    fit_day30 = fit_day30
  ),
  file.path(
    RESULTS_DIRECTORY,
    "primary_analysis_objects.rds"
  )
)

capture.output(
  sessionInfo(),
  file = file.path(
    RESULTS_DIRECTORY,
    "sessionInfo_primary.txt"
  )
)

cat("\n逐次ランドマーク主要解析完了\n")
