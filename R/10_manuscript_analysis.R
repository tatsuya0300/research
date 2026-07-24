############################################################
# 10_manuscript_analysis.R
#
# 逐次ランドマーク主要解析（Results案準拠版）
# ・pooled logistic regression
# ・患者cluster-robust SE
# ・ΔMRCI-J = 0 vs +5点の標準化確率
# ・絶対リスク差、リスク比
# ・restricted cubic spline（非線形性）
# ・ランドマーク時期別標準化リスク差（forest plot）
# ・cause-specific Cox
# ・Aalen-Johansen CIF
#
# 修正点（既存06/09との差分）:
#   - ハードコード絶対パス廃止 → rprojroot
#   - 標準化比較：観察値→+5点 ではなく Δ=0 vs Δ=+5
#   - Figure 3：線形モデルではなくnatural splineから作図
#   - ランドマークforest：ORだけでなく絶対リスク差も表示
############################################################

options(stringsAsFactors = FALSE, warn = 1)

#============================================================
# 0. パッケージ
#============================================================

required_packages <- c(
  "tidyverse",
  "rprojroot",
  "survival",
  "sandwich",
  "lmtest",
  "splines",
  "cmprsk",
  "broom",
  "scales",
  "patchwork"
)

not_installed <- setdiff(
  required_packages,
  rownames(installed.packages())
)

if (length(not_installed) > 0) {
  install.packages(not_installed, dependencies = TRUE)
}

library(tidyverse)
library(rprojroot)
library(survival)
library(sandwich)
library(lmtest)
library(splines)
library(cmprsk)
library(broom)
library(scales)
library(patchwork)

#============================================================
# 1. パス設定（rprojroot）
#============================================================

ROOT <- tryCatch(
  find_root(is_git_root | has_file("research.Rproj")),
  error = function(e) getwd()
)

DATA_DIR   <- file.path(ROOT, "data")
RESULT_DIR <- file.path(ROOT, "results")
TABLE_DIR  <- file.path(RESULT_DIR, "tables")
FIGURE_DIR <- file.path(RESULT_DIR, "figures")

dir.create(DATA_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TABLE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(FIGURE_DIR, recursive = TRUE, showWarnings = FALSE)

#-----------------------------------------------------------
# データ読み込み
#-----------------------------------------------------------

landmark_file <- file.path(DATA_DIR, "landmark_long.rds")

if (!file.exists(landmark_file)) {
  landmark_file <- file.path(DATA_DIR, "sample_landmark_long.rds")
}

if (!file.exists(landmark_file)) {
  stop("landmark_long.rds（またはsample版）が見つかりません。")
}

d <- readRDS(landmark_file)

cat("\nデータ読み込み完了:", nrow(d), "行\n")
cat("列名:", paste(names(d), collapse = ", "), "\n")

#============================================================
# 2. 必須変数チェック
#============================================================

required_variables <- c(
  "id", "landmark",
  "previous_mrci", "current_mrci", "delta_mrci",
  "age", "sex", "stroke_type", "recurrent_stroke",
  "onset_to_admission", "calendar_year",
  "fim_motor", "fim_cognitive",
  "previous_management_level",
  "premorbid_manager", "family_support", "planned_destination",
  "self_management_30d", "followup_days", "status"
)

missing_variables <- setdiff(required_variables, names(d))

if (length(missing_variables) > 0) {
  stop("不足変数: ", paste(missing_variables, collapse = ", "))
}

#============================================================
# 3. 変数型整備
#============================================================

d <- d |>
  mutate(
    id = factor(id),

    landmark = factor(
      as.character(landmark),
      levels = c("30", "60", "90", "120")
    ),

    landmark_group = factor(
      case_when(
        landmark == "30" ~ "Day 30",
        landmark == "60" ~ "Day 60",
        landmark %in% c("90", "120") ~ "Day 90/120"
      ),
      levels = c("Day 30", "Day 60", "Day 90/120")
    ),

    sex = factor(sex),
    stroke_type = factor(stroke_type),
    recurrent_stroke = factor(recurrent_stroke),
    calendar_year = factor(calendar_year),
    premorbid_manager = factor(premorbid_manager),
    family_support = factor(family_support),
    planned_destination = factor(planned_destination),

    previous_management_level = factor(
      previous_management_level, ordered = FALSE
    ),

    # delta5 = MRCI-J変化量（正＝複雑性低下）
    delta5 = delta_mrci / 5,

    self_management_30d = as.integer(self_management_30d),
    status = as.integer(status)
  ) |>
  arrange(id, landmark)

#-----------------------------------------------------------
# データ品質確認
#-----------------------------------------------------------

stopifnot(
  all(abs(d$delta_mrci - (d$previous_mrci - d$current_mrci)) < 1e-8, na.rm = TRUE)
)
stopifnot(all(d$status %in% c(0, 1, 2), na.rm = TRUE))
stopifnot(all(d$self_management_30d == as.integer(d$status == 1), na.rm = TRUE))
stopifnot(all(d$followup_days > 0 & d$followup_days <= 30, na.rm = TRUE))

cat("データ品質確認完了\n")

#============================================================
# 4. 補助関数
#============================================================

#-----------------------------------------------------------
# cluster-robust GLM
#-----------------------------------------------------------

fit_cluster_glm <- function(formula, data) {

  model_variables <- unique(c(all.vars(formula), "id"))
  model_data <- data |> select(all_of(model_variables)) |> drop_na()

  fit <- glm(
    formula,
    data = model_data,
    family = binomial(link = "logit"),
    x = TRUE, y = TRUE, model = TRUE
  )

  V <- sandwich::vcovCL(fit, cluster = model_data$id, type = "HC0")

  list(fit = fit, vcov = V, data = model_data)
}

#-----------------------------------------------------------
# ロバスト回帰表
#-----------------------------------------------------------

robust_glm_table <- function(object, exposure = NULL) {

  fit <- object$fit
  V   <- object$vcov

  ct <- lmtest::coeftest(fit, vcov. = V)

  out <- tibble(
    variable    = rownames(ct),
    coefficient = ct[, 1],
    robust_se   = ct[, 2],
    odds_ratio  = exp(ct[, 1]),
    ci_lower    = exp(ct[, 1] - qnorm(0.975) * ct[, 2]),
    ci_upper    = exp(ct[, 1] + qnorm(0.975) * ct[, 2]),
    p_value     = ct[, 4],
    n_observations = nrow(object$data),
    n_patients     = n_distinct(object$data$id)
  )

  if (!is.null(exposure)) {
    out <- out |> filter(variable %in% exposure)
  }

  out
}

#-----------------------------------------------------------
# 与えたdelta5値における標準化確率（delta method SE）
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

  standard_error <- sqrt(as.numeric(t(gradient) %*% V %*% gradient))

  list(
    estimate       = estimate,
    standard_error = standard_error,
    gradient       = gradient,
    vcov           = V
  )
}

#-----------------------------------------------------------
# ΔMRCI-J = 0点 vs +5点 の標準化比較
#-----------------------------------------------------------

standardized_0_vs_5 <- function(fit, data, vcov_matrix) {

  r0 <- standardized_probability(
    fit = fit, newdata = data,
    delta5_value = 0, vcov_matrix = vcov_matrix
  )

  r5 <- standardized_probability(
    fit = fit, newdata = data,
    delta5_value = 1, vcov_matrix = vcov_matrix
  )

  V <- r0$vcov

  # 絶対リスク差（ARD）
  rd     <- r5$estimate - r0$estimate
  grad_rd <- r5$gradient - r0$gradient
  se_rd  <- sqrt(as.numeric(t(grad_rd) %*% V %*% grad_rd))

  # リスク比（RR, logスケールでCI）
  rr        <- r5$estimate / r0$estimate
  grad_log_rr <- r5$gradient / r5$estimate - r0$gradient / r0$estimate
  se_log_rr <- sqrt(as.numeric(t(grad_log_rr) %*% V %*% grad_log_rr))

  bind_rows(
    tibble(
      estimand = "Standardized risk: delta MRCI-J = 0",
      estimate = r0$estimate,
      conf_low  = max(0, r0$estimate - qnorm(0.975) * r0$standard_error),
      conf_high = min(1, r0$estimate + qnorm(0.975) * r0$standard_error)
    ),
    tibble(
      estimand = "Standardized risk: delta MRCI-J = +5",
      estimate = r5$estimate,
      conf_low  = max(0, r5$estimate - qnorm(0.975) * r5$standard_error),
      conf_high = min(1, r5$estimate + qnorm(0.975) * r5$standard_error)
    ),
    tibble(
      estimand = "Absolute risk difference: +5 vs 0",
      estimate = rd,
      conf_low  = rd - qnorm(0.975) * se_rd,
      conf_high = rd + qnorm(0.975) * se_rd
    ),
    tibble(
      estimand = "Risk ratio: +5 vs 0",
      estimate = rr,
      conf_low  = exp(log(rr) - qnorm(0.975) * se_log_rr),
      conf_high = exp(log(rr) + qnorm(0.975) * se_log_rr)
    )
  )
}

#============================================================
# 5. ランドマーク別記述統計（表2基礎）
#============================================================

table2_landmark <- d |>
  group_by(landmark) |>
  summarise(
    risk_set              = n(),
    unique_patients       = n_distinct(id),
    age_median            = median(age, na.rm = TRUE),
    age_q1                = quantile(age, 0.25, na.rm = TRUE),
    age_q3                = quantile(age, 0.75, na.rm = TRUE),
    fim_motor_median      = median(fim_motor, na.rm = TRUE),
    fim_cognitive_median  = median(fim_cognitive, na.rm = TRUE),
    previous_mrci_median  = median(previous_mrci, na.rm = TRUE),
    current_mrci_median   = median(current_mrci, na.rm = TRUE),
    delta_mrci_median     = median(delta_mrci, na.rm = TRUE),
    delta_mrci_q1         = quantile(delta_mrci, 0.25, na.rm = TRUE),
    delta_mrci_q3         = quantile(delta_mrci, 0.75, na.rm = TRUE),
    self_management       = sum(status == 1),
    self_management_percent = 100 * mean(status == 1),
    competing_event       = sum(status == 2),
    competing_event_percent = 100 * mean(status == 2),
    event_free_30d        = sum(status == 0),
    .groups = "drop"
  )

write_csv(table2_landmark, file.path(TABLE_DIR, "Table02_landmark_summary.csv"))
print(table2_landmark)

# MRCI-J変化の方向別集計
delta_direction <- d |>
  summarise(
    patient_landmark_observations = n(),
    decreased_n   = sum(delta_mrci > 0, na.rm = TRUE),
    unchanged_n   = sum(delta_mrci == 0, na.rm = TRUE),
    increased_n   = sum(delta_mrci < 0, na.rm = TRUE),
    decrease_5_or_more_n = sum(delta_mrci >= 5, na.rm = TRUE),
    min_delta     = min(delta_mrci, na.rm = TRUE),
    max_delta     = max(delta_mrci, na.rm = TRUE)
  )

write_csv(delta_direction, file.path(TABLE_DIR, "MRCI_change_summary.csv"))

#============================================================
# 6. モデル定義
#============================================================

formula_unadjusted <- self_management_30d ~ delta5

formula_model1 <- self_management_30d ~
  delta5 + age + sex + stroke_type +
  onset_to_admission + previous_mrci +
  landmark + calendar_year

formula_model2 <- self_management_30d ~
  delta5 + age + sex + stroke_type +
  recurrent_stroke + onset_to_admission +
  previous_mrci + fim_motor + fim_cognitive +
  previous_management_level + premorbid_manager +
  family_support + planned_destination +
  landmark + calendar_year

# モデル3（追加変数がある場合のみ）
model3_additional <- c("clinical_judgement", "days_to_planned_discharge")
has_model3 <- all(model3_additional %in% names(d))

if (has_model3) {
  formula_model3 <- update(
    formula_model2,
    . ~ . + clinical_judgement + days_to_planned_discharge
  )
}

#============================================================
# 7. pooled logistic regression
#============================================================

cat("\n=== モデル推定 ===\n")

fit_unadjusted <- fit_cluster_glm(formula_unadjusted, d)
fit_model1     <- fit_cluster_glm(formula_model1, d)
fit_model2     <- fit_cluster_glm(formula_model2, d)

model_list <- list(
  "Unadjusted"       = fit_unadjusted,
  "Model 1"          = fit_model1,
  "Model 2: primary" = fit_model2
)

if (has_model3) {
  fit_model3 <- fit_cluster_glm(formula_model3, d)
  model_list[["Model 3: supplementary"]] <- fit_model3
}

#============================================================
# 8. 表3：主要解析結果
#============================================================

cat("\n=== 表3 計算 ===\n")

table3 <- imap_dfr(model_list, function(object, model_name) {

  or_result <- robust_glm_table(object, exposure = "delta5")
  stan_res <- standardized_0_vs_5(
    fit = object$fit, data = object$data, vcov_matrix = object$vcov
  )

  risk0 <- stan_res |> filter(estimand == "Standardized risk: delta MRCI-J = 0")
  risk5 <- stan_res |> filter(estimand == "Standardized risk: delta MRCI-J = +5")
  rd    <- stan_res |> filter(estimand == "Absolute risk difference: +5 vs 0")
  rr    <- stan_res |> filter(estimand == "Risk ratio: +5 vs 0")

  tibble(
    model            = model_name,
    n_observations   = nrow(object$data),
    n_patients       = n_distinct(object$data$id),
    odds_ratio       = or_result$odds_ratio,
    or_ci_lower      = or_result$ci_lower,
    or_ci_upper      = or_result$ci_upper,
    p_value          = or_result$p_value,
    risk_delta0      = risk0$estimate,
    risk_delta0_lower = risk0$conf_low,
    risk_delta0_upper = risk0$conf_high,
    risk_delta5      = risk5$estimate,
    risk_delta5_lower = risk5$conf_low,
    risk_delta5_upper = risk5$conf_high,
    absolute_risk_difference = rd$estimate,
    ard_ci_lower     = rd$conf_low,
    ard_ci_upper     = rd$conf_high,
    risk_ratio       = rr$estimate,
    rr_ci_lower      = rr$conf_low,
    rr_ci_upper      = rr$conf_high
  )
})

write_csv(table3, file.path(TABLE_DIR, "Table03_primary_analysis.csv"))
print(table3)

#============================================================
# 9. 非線形性（natural cubic spline）
#============================================================

cat("\n=== 非線形性評価 ===\n")

formula_spline <- update(
  formula_model2,
  . ~ . - delta5 + splines::ns(delta5, df = 3)
)

fit_spline <- fit_cluster_glm(formula_spline, d)

# LRT（cluster-robustではないため補助的指標）
nonlinearity_lrt <- anova(fit_model2$fit, fit_spline$fit, test = "LRT")
capture.output(nonlinearity_lrt, file = file.path(TABLE_DIR, "nonlinearity_LRT.txt"))
print(nonlinearity_lrt)

#-----------------------------------------------------------
# スプライン予測曲線
#-----------------------------------------------------------

exposure_limits <- quantile(
  fit_spline$data$delta_mrci,
  probs = c(0.01, 0.99), na.rm = TRUE
)

delta_grid <- seq(exposure_limits[1], exposure_limits[2], length.out = 150)

spline_curve <- map_dfr(delta_grid, function(x) {

  p <- standardized_probability(
    fit = fit_spline$fit,
    newdata = fit_spline$data,
    delta5_value = x / 5,
    vcov_matrix = fit_spline$vcov
  )

  tibble(
    delta_mrci  = x,
    probability = p$estimate,
    conf_low    = max(0, p$estimate - qnorm(0.975) * p$standard_error),
    conf_high   = min(1, p$estimate + qnorm(0.975) * p$standard_error)
  )
})

write_csv(spline_curve, file.path(TABLE_DIR, "Figure03_spline_predictions.csv"))
cat("スプライン予測曲線作成完了\n")

#============================================================
# 10. Figure 3：スプライン曲線
#============================================================

cat("\n=== Figure 3 作成 ===\n")

theme_set(
  theme_bw(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position  = "bottom"
    )
)

figure3 <- ggplot(spline_curve, aes(x = delta_mrci, y = probability)) +
  geom_ribbon(
    aes(ymin = conf_low, ymax = conf_high),
    fill = "#5DADE2", alpha = 0.25
  ) +
  geom_line(colour = "#1B4F72", linewidth = 1) +
  geom_vline(xintercept = 0, linetype = 2, colour = "grey35") +
  geom_rug(
    data = fit_spline$data,
    aes(x = delta_mrci),
    inherit.aes = FALSE,
    sides = "b", alpha = 0.15
  ) +
  scale_y_continuous(labels = label_percent(accuracy = 1)) +
  labs(
    x = "直前期間のMRCI-J低下量（正＝複雑性低下、負＝複雑性増加）",
    y = "標準化30日完全自己管理移行確率",
    title = "MRCI-J低下量と完全内服自己管理移行との関連",
    subtitle = "Natural cubic spline、患者単位cluster-robust 95% CI"
  )

ggsave(
  file.path(FIGURE_DIR, "Figure03_MRCI_spline_probability.png"),
  figure3, width = 8, height = 6, dpi = 320, bg = "white"
)
cat("Figure 3 保存完了\n")

#============================================================
# 11. ランドマーク時期との交互作用
#============================================================

cat("\n=== 交互作用評価 ===\n")

formula_interaction <- update(
  formula_model2,
  . ~ . - delta5 - landmark + delta5 * landmark_group
)

fit_interaction <- fit_cluster_glm(formula_interaction, d)

interaction_terms <- grep(
  "delta5:landmark_group",
  names(coef(fit_interaction$fit)),
  value = TRUE
)

# Wald検定（cluster-robust）
if (length(interaction_terms) > 0 && requireNamespace("car", quietly = TRUE)) {
  interaction_wald <- car::linearHypothesis(
    fit_interaction$fit,
    interaction_terms,
    vcov. = fit_interaction$vcov,
    test = "Chisq"
  )
  capture.output(
    interaction_wald,
    file = file.path(TABLE_DIR, "landmark_interaction_Wald_test.txt")
  )
  print(interaction_wald)
}

#-----------------------------------------------------------
# 時期別標準化絶対リスク差
#-----------------------------------------------------------

landmark_effects <- map_dfr(
  levels(fit_interaction$data$landmark_group),
  function(group_name) {

    subgroup <- fit_interaction$data |>
      filter(landmark_group == group_name)

    result <- standardized_0_vs_5(
      fit = fit_interaction$fit,
      data = subgroup,
      vcov_matrix = fit_interaction$vcov
    ) |>
      filter(estimand == "Absolute risk difference: +5 vs 0")

    tibble(
      landmark_group = group_name,
      estimate = result$estimate,
      conf_low  = result$conf_low,
      conf_high = result$conf_high
    )
  }
)

write_csv(landmark_effects, file.path(TABLE_DIR, "landmark_specific_absolute_risk_difference.csv"))
print(landmark_effects)

#-----------------------------------------------------------
# Figure 4：ランドマークForest plot（絶対リスク差）
#-----------------------------------------------------------

figure4 <- ggplot(landmark_effects, aes(x = estimate, y = fct_rev(factor(landmark_group)))) +
  geom_vline(xintercept = 0, linetype = 2, colour = "grey40") +
  geom_errorbarh(aes(xmin = conf_low, xmax = conf_high), height = 0.15) +
  geom_point(size = 3, colour = "#1B4F72") +
  scale_x_continuous(labels = label_percent(accuracy = 1)) +
  labs(
    x = "MRCI-J 5点低下に対応する標準化絶対リスク差",
    y = NULL,
    title = "ランドマーク時期別の関連",
    subtitle = "点推定値および95%信頼区間"
  )

ggsave(
  file.path(FIGURE_DIR, "Figure04_landmark_forest_ARD.png"),
  figure4, width = 8, height = 5, dpi = 320, bg = "white"
)
cat("Figure 4 保存完了\n")

#============================================================
# 12. cause-specific Cox回帰
#============================================================

cat("\n=== Cause-specific Cox ===\n")

cox_vars <- unique(c("followup_days", "status", "id", all.vars(formula_model2)[-1]))
cox_data <- d |> select(all_of(cox_vars)) |> drop_na()

fit_cs_cox <- coxph(
  Surv(followup_days, status == 1) ~
    delta5 + age + sex + stroke_type +
    recurrent_stroke + onset_to_admission +
    previous_mrci + fim_motor + fim_cognitive +
    previous_management_level + premorbid_manager +
    family_support + planned_destination +
    landmark + calendar_year + cluster(id),
  data = cox_data, ties = "efron", x = TRUE, y = TRUE
)

cox_summary <- summary(fit_cs_cox)

cox_table <- tibble(
  variable     = rownames(cox_summary$coefficients),
  coefficient  = cox_summary$coefficients[, "coef"],
  robust_se    = cox_summary$coefficients[, "robust se"],
  hazard_ratio = cox_summary$conf.int[, "exp(coef)"],
  ci_lower     = cox_summary$conf.int[, "lower .95"],
  ci_upper     = cox_summary$conf.int[, "upper .95"],
  p_value      = cox_summary$coefficients[, "Pr(>|z|)"]
)

write_csv(cox_table, file.path(TABLE_DIR, "cause_specific_cox.csv"))
print(cox_table |> filter(variable == "delta5"))

#============================================================
# 13. Aalen-Johansen CIF（ランドマーク別）
#============================================================

cat("\n=== Aalen-Johansen CIF ===\n")

cif_by_landmark <- map(
  levels(d$landmark),
  function(lm_value) {
    dlm <- d |> filter(landmark == lm_value)
    cmprsk::cuminc(
      ftime = dlm$followup_days,
      fstatus = dlm$status,
      cencode = 0
    )
  }
)

names(cif_by_landmark) <- levels(d$landmark)

saveRDS(cif_by_landmark, file.path(RESULT_DIR, "cumulative_incidence_by_landmark.rds"))
cat("CIF保存完了\n")

#============================================================
# 14. 欠測割合
#============================================================

cat("\n=== 欠測集計 ===\n")

missing_summary <- tibble(
  variable       = names(d),
  missing_n      = map_int(d, ~ sum(is.na(.x))),
  missing_percent = map_dbl(d, ~ 100 * mean(is.na(.x)))
) |>
  arrange(desc(missing_percent))

write_csv(missing_summary, file.path(TABLE_DIR, "missing_data_summary.csv"))
print(missing_summary |> filter(missing_n > 0))

#============================================================
# 15. 解析オブジェクト保存
#============================================================

cat("\n=== 解析オブジェクト保存 ===\n")

saveRDS(
  list(
    fit_unadjusted = fit_unadjusted,
    fit_model1     = fit_model1,
    fit_model2     = fit_model2,
    fit_model3     = if (has_model3) fit_model3 else NULL,
    fit_spline     = fit_spline,
    fit_interaction = fit_interaction,
    fit_cs_cox     = fit_cs_cox,
    table3         = table3,
    spline_curve   = spline_curve,
    landmark_effects = landmark_effects
  ),
  file.path(RESULT_DIR, "manuscript_analysis_objects.rds")
)

capture.output(
  sessionInfo(),
  file = file.path(RESULT_DIR, "sessionInfo_manuscript.txt")
)

cat("\n========================================\n")
cat("主要解析および図示が完了しました。\n")
cat("表:", TABLE_DIR, "\n")
cat("図:", FIGURE_DIR, "\n")
cat("========================================\n")
