############################################################
# 13_mrci_discharge_primary_analysis.R
#
# 入院時MRCI-Jと退院時完全内服自己管理との関連
#
# 主要解析（modified Poisson + robust SE）
# 副次解析（spline、服薬エラー、予測性能）
# 感度分析（サブグループ、MRCI定義変更）
############################################################

options(stringsAsFactors = FALSE, warn = 1)

required_packages <- c(
  "rprojroot", "sandwich", "lmtest", "splines",
  "ggplot2", "pROC", "MASS"
)

not_installed <- setdiff(
  required_packages,
  rownames(installed.packages())
)

if (length(not_installed) > 0) {
  install.packages(not_installed, dependencies = TRUE)
}

library(sandwich)
library(lmtest)
library(splines)
library(ggplot2)
library(pROC)
library(MASS)

ROOT <- tryCatch(
  rprojroot::find_root(
    rprojroot::is_git_root |
      rprojroot::has_file("research.Rproj")
  ),
  error = function(e) getwd()
)

DATA_DIR <- file.path(ROOT, "data")
RESULT_DIR <- file.path(ROOT, "results")
TABLE_DIR <- file.path(RESULT_DIR, "tables")
FIGURE_DIR <- file.path(RESULT_DIR, "figures")

dir.create(TABLE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(FIGURE_DIR, recursive = TRUE, showWarnings = FALSE)

data_file <- file.path(DATA_DIR, "mrci_discharge.rds")

if (!file.exists(data_file)) {
  data_file <- file.path(DATA_DIR, "sample_mrci_discharge.rds")
}

if (!file.exists(data_file)) {
  stop("mrci_discharge.rdsまたはsample版がありません。")
}

d <- readRDS(data_file)

#-----------------------------------------------------------
# 必須変数
#-----------------------------------------------------------

required_variables <- c(
  "id", "discharge_complete_self", "mrci0",
  "age", "sex", "premorbid_manager", "premorbid_mrs",
  "stroke_type", "fim_motor0", "fim_cognitive0",
  "family_support0", "ward", "admission_year"
)

missing_variables <- setdiff(required_variables, names(d))

if (length(missing_variables) > 0) {
  stop(
    "不足変数: ",
    paste(missing_variables, collapse = ", ")
  )
}

#-----------------------------------------------------------
# 型整備
#-----------------------------------------------------------

d$id <- factor(d$id)
d$sex <- factor(d$sex)
d$stroke_type <- factor(d$stroke_type)
d$premorbid_manager <- factor(d$premorbid_manager)
d$premorbid_mrs <- factor(d$premorbid_mrs)
d$family_support0 <- factor(d$family_support0)
d$ward <- factor(d$ward)
d$admission_year <- factor(d$admission_year)

d$discharge_complete_self <- as.integer(
  d$discharge_complete_self
)

d$mrci5 <- d$mrci0 / 5
d$mrci_a5 <- d$mrci_a0 / 5
d$mrci_b5 <- d$mrci_b0 / 5
d$mrci_c5 <- d$mrci_c0 / 5

stopifnot(
  all(d$discharge_complete_self %in% c(0, 1)),
  all(d$mrci0 >= 0, na.rm = TRUE)
)

#-----------------------------------------------------------
# 記述統計・欠測
#-----------------------------------------------------------

flow <- data.frame(
  total_n = nrow(d),
  complete_self_n = sum(d$discharge_complete_self == 1),
  non_complete_self_n = sum(d$discharge_complete_self == 0),
  initial_complete_self_n = sum(d$initial_complete_self == 1),
  trial_started_n = sum(d$trial_started == 1),
  mrci_mean = mean(d$mrci0),
  mrci_sd = sd(d$mrci0),
  mrci_median = median(d$mrci0),
  mrci_q1 = quantile(d$mrci0, 0.25),
  mrci_q3 = quantile(d$mrci0, 0.75)
)

write.csv(
  flow,
  file.path(TABLE_DIR, "cohort_flow.csv"),
  row.names = FALSE
)

missing_table <- data.frame(
  variable = names(d),
  missing_n = vapply(d, function(x) sum(is.na(x)), numeric(1)),
  missing_percent = vapply(
    d,
    function(x) 100 * mean(is.na(x)),
    numeric(1)
  )
)

write.csv(
  missing_table,
  file.path(TABLE_DIR, "missing_data.csv"),
  row.names = FALSE
)

#-----------------------------------------------------------
# 修正Poisson＋ロバスト標準誤差
#-----------------------------------------------------------

fit_modified_poisson <- function(formula, data) {

  model_variables <- all.vars(formula)

  md <- data[
    complete.cases(data[, model_variables, drop = FALSE]),
    ,
    drop = FALSE
  ]

  fit <- glm(
    formula,
    data = md,
    family = poisson(link = "log"),
    x = TRUE,
    y = TRUE,
    model = TRUE
  )

  V <- sandwich::vcovHC(
    fit,
    type = "HC0"
  )

  list(
    fit = fit,
    vcov = V,
    data = md
  )
}

robust_table <- function(object) {

  ct <- lmtest::coeftest(
    object$fit,
    vcov. = object$vcov
  )

  data.frame(
    variable = rownames(ct),
    coefficient = ct[, 1],
    robust_se = ct[, 2],
    relative_risk = exp(ct[, 1]),
    ci_lower = exp(ct[, 1] - 1.96 * ct[, 2]),
    ci_upper = exp(ct[, 1] + 1.96 * ct[, 2]),
    p_value = ct[, 4],
    n = nrow(object$data),
    events = sum(object$data$discharge_complete_self),
    row.names = NULL
  )
}

#-----------------------------------------------------------
# 段階的モデル
#-----------------------------------------------------------

formula_model1 <-
  discharge_complete_self ~ mrci5

formula_model2 <-
  discharge_complete_self ~
  mrci5 +
  age +
  sex +
  premorbid_manager +
  premorbid_mrs +
  stroke_type

formula_model3 <-
  discharge_complete_self ~
  mrci5 +
  age +
  sex +
  premorbid_manager +
  premorbid_mrs +
  stroke_type +
  fim_motor0 +
  fim_cognitive0 +
  family_support0 +
  ward +
  admission_year

formula_model4 <-
  update(
    formula_model3,
    . ~ . +
      nihss0 +
      aphasia +
      neglect +
      comorbidity_index
  )

fits <- list(
  Model1 = fit_modified_poisson(formula_model1, d),
  Model2 = fit_modified_poisson(formula_model2, d),
  Model3 = fit_modified_poisson(formula_model3, d),
  Model4 = fit_modified_poisson(formula_model4, d)
)

model_results <- do.call(
  rbind,
  lapply(names(fits), function(model_name) {
    x <- robust_table(fits[[model_name]])
    x$model <- model_name
    x
  })
)

write.csv(
  model_results,
  file.path(TABLE_DIR, "modified_poisson_models.csv"),
  row.names = FALSE
)

primary_result <- subset(
  model_results,
  model == "Model3" & variable == "mrci5"
)

print(primary_result)

#-----------------------------------------------------------
# 標準化確率
#
# Poisson log-linkなので勾配は X * p
# logisticの X * p * (1-p) ではない点に注意
#-----------------------------------------------------------

standardized_probability <- function(
  object,
  mrci_value
) {

  fit <- object$fit
  V <- object$vcov
  nd <- object$data

  nd$mrci5 <- mrci_value / 5

  p <- as.numeric(
    predict(fit, newdata = nd, type = "response")
  )

  X <- model.matrix(
    delete.response(terms(fit)),
    data = nd,
    contrasts.arg = fit$contrasts,
    xlev = fit$xlevels
  )

  beta_names <- names(coef(fit))
  X <- X[, beta_names, drop = FALSE]
  V <- V[beta_names, beta_names, drop = FALSE]

  gradient <- colMeans(X * p)

  estimate <- mean(p)

  variance <- as.numeric(
    t(gradient) %*% V %*% gradient
  )

  data.frame(
    mrci = mrci_value,
    probability = estimate,
    standard_error = sqrt(variance),
    ci_lower = max(0, estimate - 1.96 * sqrt(variance)),
    ci_upper = min(1, estimate + 1.96 * sqrt(variance))
  )
}

mrci_values <- unique(
  as.numeric(
    quantile(
      fits$Model3$data$mrci0,
      probs = c(0.10, 0.25, 0.50, 0.75, 0.90)
    )
  )
)

standardized_risks <- do.call(
  rbind,
  lapply(
    mrci_values,
    function(x) {
      standardized_probability(
        fits$Model3,
        mrci_value = x
      )
    }
  )
)

write.csv(
  standardized_risks,
  file.path(TABLE_DIR, "standardized_probabilities.csv"),
  row.names = FALSE
)

# 代表値間の絶対リスク差
risk_difference <- data.frame(
  mrci_low = standardized_risks$mrci[1],
  mrci_high = standardized_risks$mrci[nrow(standardized_risks)],
  probability_low = standardized_risks$probability[1],
  probability_high =
    standardized_risks$probability[nrow(standardized_risks)],
  risk_difference =
    standardized_risks$probability[nrow(standardized_risks)] -
    standardized_risks$probability[1]
)

write.csv(
  risk_difference,
  file.path(TABLE_DIR, "standardized_risk_difference.csv"),
  row.names = FALSE
)

#-----------------------------------------------------------
# 非線形性：natural cubic spline
#-----------------------------------------------------------

formula_spline <-
  discharge_complete_self ~
  ns(mrci0, df = 3) +
  age +
  sex +
  premorbid_manager +
  premorbid_mrs +
  stroke_type +
  fim_motor0 +
  fim_cognitive0 +
  family_support0 +
  ward +
  admission_year

fit_spline <- fit_modified_poisson(
  formula_spline,
  d
)

# Wald検定：MRCIスプライン項全体
spline_terms <- grep(
  "^ns\\(mrci0",
  names(coef(fit_spline$fit))
)

wald_stat <- as.numeric(
  t(coef(fit_spline$fit)[spline_terms]) %*%
    solve(fit_spline$vcov[spline_terms, spline_terms]) %*%
    coef(fit_spline$fit)[spline_terms]
)

spline_p <- pchisq(
  wald_stat,
  df = length(spline_terms),
  lower.tail = FALSE
)

write.csv(
  data.frame(
    wald_chisq = wald_stat,
    df = length(spline_terms),
    p_value = spline_p
  ),
  file.path(TABLE_DIR, "spline_overall_test.csv"),
  row.names = FALSE
)

#-----------------------------------------------------------
# Section A、B、C：別モデル
#-----------------------------------------------------------

section_results <- do.call(
  rbind,
  lapply(
    c("mrci_a5", "mrci_b5", "mrci_c5"),
    function(exposure) {

      f <- update(
        formula_model3,
        as.formula(
          paste(
            ". ~ . - mrci5 +",
            exposure
          )
        )
      )

      fit <- fit_modified_poisson(f, d)
      result <- robust_table(fit)

      result[
        result$variable == exposure,
        ,
        drop = FALSE
      ]
    }
  )
)

write.csv(
  section_results,
  file.path(TABLE_DIR, "mrci_sections.csv"),
  row.names = FALSE
)

#-----------------------------------------------------------
# 自己管理試行開始
#-----------------------------------------------------------

formula_trial <-
  trial_started ~
  mrci5 +
  age +
  sex +
  premorbid_manager +
  premorbid_mrs +
  stroke_type +
  fim_motor0 +
  fim_cognitive0 +
  family_support0 +
  ward +
  admission_year

fit_trial <- fit_modified_poisson(
  formula_trial,
  d
)

write.csv(
  robust_table(fit_trial),
  file.path(TABLE_DIR, "trial_started_analysis.csv"),
  row.names = FALSE
)

#-----------------------------------------------------------
# 服薬エラー件数
# 自己管理試行または完全自己管理を開始した患者に限定
#-----------------------------------------------------------

error_data <- subset(
  d,
  trial_started == 1 &
    self_management_patient_days > 0 &
    !is.na(medication_error_count)
)

error_formula <-
  medication_error_count ~
  mrci5 +
  age +
  sex +
  stroke_type +
  fim_motor0 +
  fim_cognitive0 +
  premorbid_manager +
  offset(log(self_management_patient_days))

fit_error_poisson <- glm(
  error_formula,
  data = error_data,
  family = poisson(link = "log")
)

dispersion <- sum(
  residuals(fit_error_poisson, type = "pearson")^2
) / df.residual(fit_error_poisson)

if (dispersion > 1.5) {

  fit_error <- MASS::glm.nb(
    error_formula,
    data = error_data
  )

  error_model_type <- "Negative binomial"

} else {

  fit_error <- fit_error_poisson
  error_model_type <- "Poisson"
}

error_table <- data.frame(
  variable = names(coef(fit_error)),
  coefficient = coef(fit_error),
  rate_ratio = exp(coef(fit_error)),
  ci_lower = exp(
    coef(fit_error) -
      1.96 * sqrt(diag(vcov(fit_error)))
  ),
  ci_upper = exp(
    coef(fit_error) +
      1.96 * sqrt(diag(vcov(fit_error)))
  ),
  model = error_model_type,
  dispersion = dispersion
)

write.csv(
  error_table,
  file.path(TABLE_DIR, "medication_error_count.csv"),
  row.names = FALSE
)

#-----------------------------------------------------------
# MRCI-J変化量：探索的解析
# delta_mrci = 退院前 - 入院時
#-----------------------------------------------------------

d$delta_mrci5 <- d$delta_mrci / 5
d$mrci_discharge5 <- d$mrci_discharge / 5

change_formula <-
  discharge_complete_self ~
  delta_mrci5 +
  mrci5 +
  age +
  sex +
  premorbid_manager +
  premorbid_mrs +
  stroke_type +
  fim_motor0 +
  fim_cognitive0 +
  family_support0 +
  ward +
  admission_year

fit_change <- fit_modified_poisson(
  change_formula,
  d
)

write.csv(
  robust_table(fit_change),
  file.path(TABLE_DIR, "mrci_change_exploratory.csv"),
  row.names = FALSE
)

#-----------------------------------------------------------
# 予測能：探索的解析
# 有効な確率を得るためlogistic regressionを使用
#-----------------------------------------------------------

prediction_data <- fits$Model3$data

formula_prediction_base <-
  discharge_complete_self ~
  age +
  sex +
  premorbid_manager +
  premorbid_mrs +
  stroke_type +
  fim_motor0 +
  fim_cognitive0 +
  family_support0 +
  ward +
  admission_year

prediction_models <- list(
  Background_function =
    glm(
      formula_prediction_base,
      data = prediction_data,
      family = binomial()
    ),

  Background_medication_count =
    glm(
      update(
        formula_prediction_base,
        . ~ . + medication_count0
      ),
      data = prediction_data,
      family = binomial()
    ),

  Background_MRCI =
    glm(
      update(
        formula_prediction_base,
        . ~ . + mrci5
      ),
      data = prediction_data,
      family = binomial()
    )
)

prediction_performance <- do.call(
  rbind,
  lapply(
    names(prediction_models),
    function(model_name) {

      fit <- prediction_models[[model_name]]
      y <- model.response(model.frame(fit))
      p <- predict(fit, type = "response")
      lp <- predict(fit, type = "link")

      roc_object <- pROC::roc(
        response = y,
        predictor = p,
        quiet = TRUE
      )

      calibration_intercept_fit <- glm(
        y ~ 1,
        offset = lp,
        family = binomial()
      )

      calibration_slope_fit <- glm(
        y ~ lp,
        family = binomial()
      )

      data.frame(
        model = model_name,
        c_statistic = as.numeric(pROC::auc(roc_object)),
        brier_score = mean((y - p)^2),
        calibration_intercept =
          coef(calibration_intercept_fit)[1],
        calibration_slope =
          coef(calibration_slope_fit)["lp"],
        AIC = AIC(fit),
        BIC = BIC(fit)
      )
    }
  )
)

write.csv(
  prediction_performance,
  file.path(TABLE_DIR, "prediction_performance_apparent.csv"),
  row.names = FALSE
)

#-----------------------------------------------------------
# 感度分析
#-----------------------------------------------------------

run_sensitivity <- function(
  data,
  label,
  exposure = "mrci5"
) {

  formula_text <- paste(
    "discharge_complete_self ~",
    exposure,
    "+ age + sex + premorbid_manager + premorbid_mrs +",
    "stroke_type + fim_motor0 + fim_cognitive0 +",
    "family_support0 + ward + admission_year"
  )

  fit <- fit_modified_poisson(
    as.formula(formula_text),
    data
  )

  result <- robust_table(fit)
  result <- result[result$variable == exposure, ]
  result$analysis <- label

  result
}

sensitivity_results <- rbind(
  run_sensitivity(
    d,
    "Primary complete-case"
  ),

  run_sensitivity(
    subset(d, initial_complete_self == 0),
    "Exclude complete self-management at admission"
  ),

  run_sensitivity(
    subset(d, trial_opportunity_unavailable == 0),
    "Exclude no trial opportunity"
  ),

  run_sensitivity(
    subset(d, home_discharge == 1),
    "Home discharge only"
  ),

  run_sensitivity(
    subset(d, ward == "Rehabilitation"),
    "Rehabilitation ward only"
  ),

  run_sensitivity(
    subset(d, premorbid_manager == "Self"),
    "Premorbid self-management only"
  ),

  run_sensitivity(
    subset(d, dementia == 0),
    "No dementia diagnosis"
  ),

  run_sensitivity(
    subset(d, prescription_confirmed_72h == 1),
    "Prescription confirmed within 72 hours"
  ),

  run_sensitivity(
    subset(d, discharge_rx_consistent == 1),
    "Discharge prescription consistent"
  ),

  run_sensitivity(
    d,
    "MRCI including PRN",
    exposure = "I(mrci_prn0 / 5)"
  ),

  run_sensitivity(
    d,
    "All-prescription MRCI",
    exposure = "I(mrci_all0 / 5)"
  )
)

write.csv(
  sensitivity_results,
  file.path(TABLE_DIR, "sensitivity_analyses.csv"),
  row.names = FALSE
)

#-----------------------------------------------------------
# ロジスティック回帰による感度分析
#-----------------------------------------------------------

logistic_fit <- glm(
  formula_model3,
  data = d,
  family = binomial(),
  na.action = na.exclude
)

logistic_table <- data.frame(
  variable = names(coef(logistic_fit)),
  odds_ratio = exp(coef(logistic_fit)),
  ci_lower = exp(
    coef(logistic_fit) -
      1.96 * sqrt(diag(vcov(logistic_fit)))
  ),
  ci_upper = exp(
    coef(logistic_fit) +
      1.96 * sqrt(diag(vcov(logistic_fit)))
  )
)

write.csv(
  logistic_table,
  file.path(TABLE_DIR, "logistic_sensitivity.csv"),
  row.names = FALSE
)

#-----------------------------------------------------------
# 保存
#-----------------------------------------------------------

saveRDS(
  list(
    fits = fits,
    fit_spline = fit_spline,
    standardized_risks = standardized_risks,
    prediction_models = prediction_models,
    sensitivity_results = sensitivity_results
  ),
  file.path(
    RESULT_DIR,
    "mrci_discharge_analysis_objects.rds"
  )
)

capture.output(
  sessionInfo(),
  file = file.path(
    RESULT_DIR,
    "sessionInfo_mrci_discharge.txt"
  )
)

cat("\n主要解析完了\n")
