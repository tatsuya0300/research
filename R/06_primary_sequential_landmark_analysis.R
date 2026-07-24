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

# プロジェクトルートの検出（rprojroot）
if (requireNamespace("rprojroot", quietly = TRUE)) {
  library(rprojroot)
  ROOT <- find_root(is_git_root | has_file("research.Rproj"))
} else {
  ROOT <- getwd()
}

DATA_DIRECTORY    <- file.path(ROOT, "data")
RESULTS_DIRECTORY <- file.path(ROOT, "results")
FIGURE_DIRECTORY  <- file.path(RESULTS_DIRECTORY, "figures")
TABLE_DIRECTORY   <- file.path(RESULTS_DIRECTORY, "tables")

dir.create(DATA_DIRECTORY,    recursive = TRUE, showWarnings = FALSE)
dir.create(RESULTS_DIRECTORY, recursive = TRUE, showWarnings = FALSE)
dir.create(FIGURE_DIRECTORY,  recursive = TRUE, showWarnings = FALSE)
dir.create(TABLE_DIRECTORY,   recursive = TRUE, showWarnings = FALSE)

cat(
  "\nRoot:", ROOT,
  "\nData:", DATA_DIRECTORY,
  "\nResults:", RESULTS_DIRECTORY,
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
# 標準化予測関数（Results案準拠版）
#
# ΔMRCI-J = 0点 vs +5点（delta5 = 0 vs delta5 = 1）
# の標準化比較。delta method SE、患者cluster-robust covariance
#-----------------------------------------------------------

standardized_probability <- function(fit, newdata, delta5_value, vcov_matrix) {

  nd <- newdata
  nd$delta5 <- delta5_value

  p <- predict(fit, newdata = nd, type = "response")

  beta <- coef(fit)
  keep <- !is.na(beta)
  beta_names <- names(beta)[keep]

  X <- model.matrix(
    delete.response(terms(fit)),
    data = nd,
    contrasts.arg = fit$contrasts,
    xlev = fit$xlevels
  )
  X <- X[, beta_names, drop = FALSE]
  V <- vcov_matrix[beta_names, beta_names, drop = FALSE]

  gradient <- colMeans(X * as.numeric(p * (1 - p)))
  estimate <- mean(p)
  se <- sqrt(as.numeric(t(gradient) %*% V %*% gradient))

  list(estimate = estimate, standard_error = se, gradient = gradient)
}

standardized_0_vs_5 <- function(fit, data, vcov_matrix) {

  r0 <- standardized_probability(fit, data, 0, vcov_matrix)
  r5 <- standardized_probability(fit, data, 1, vcov_matrix)

  V <- vcov_matrix[names(coef(fit))[!is.na(coef(fit))],
                   names(coef(fit))[!is.na(coef(fit))],
                   drop = FALSE]

  # 絶対リスク差
  rd <- r5$estimate - r0$estimate
  grad_rd <- r5$gradient - r0$gradient
  se_rd <- sqrt(as.numeric(t(grad_rd) %*% V %*% grad_rd))

  # リスク比（log RRのSE）
  rr <- r5$estimate / r0$estimate
  grad_log_rr <- r5$gradient / r5$estimate - r0$gradient / r0$estimate
  se_log_rr <- sqrt(as.numeric(t(grad_log_rr) %*% V %*% grad_log_rr))

  data.frame(
    estimand = c(
      "Standardized risk: delta MRCI-J = 0",
      "Standardized risk: delta MRCI-J = +5",
      "Absolute risk difference: +5 vs 0",
      "Risk ratio: +5 vs 0"
    ),
    estimate = c(r0$estimate, r5$estimate, rd, rr),
    ci_lower = c(
      max(0, r0$estimate - 1.96 * r0$standard_error),
      max(0, r5$estimate - 1.96 * r5$standard_error),
      rd - 1.96 * se_rd,
      exp(log(rr) - 1.96 * se_log_rr)
    ),
    ci_upper = c(
      min(1, r0$estimate + 1.96 * r0$standard_error),
      min(1, r5$estimate + 1.96 * r5$standard_error),
      rd + 1.96 * se_rd,
      exp(log(rr) + 1.96 * se_log_rr)
    ),
    stringsAsFactors = FALSE
  )
}

standardized_result <- standardized_0_vs_5(
  fit = fit_main,
  data = d,
  vcov_matrix = main_vcov
)

write.csv(
  standardized_result,
  file.path(TABLE_DIRECTORY, "main_standardized_risk.csv"),
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
