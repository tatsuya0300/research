############################################################
# 11_medication_error_manuscript_analysis.R
#
# 完全内服自己管理開始後の患者起因服薬エラー解析
# （Results案準拠版）
#
# 修正点（既存07との差分）:
#   - ハードコード絶対パス廃止 → rprojroot
#   - 確認頻度・検出方法を調整候補に含める
#   - 処方安定期間を「31+」対応（NAも含む）
#   - Figure 5：CIF + 標準化 binary estimand
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
  "cmprsk",
  "MASS",
  "broom",
  "scales"
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
library(cmprsk)
library(MASS)
library(broom)
library(scales)

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

error_file <- file.path(DATA_DIR, "medication_error.rds")

if (!file.exists(error_file)) {
  error_file <- file.path(DATA_DIR, "sample_medication_error.rds")
}

if (!file.exists(error_file)) {
  stop("medication_error.rds（またはsample版）が見つかりません。")
}

e <- readRDS(error_file)

cat("\nデータ読み込み完了:", nrow(e), "症例\n")

#============================================================
# 2. 変数名互換
#============================================================

if (!"brought_in_hospital_mix" %in% names(e) &&
      "brought_in_and_hospital_mix" %in% names(e)) {
  names(e)[names(e) == "brought_in_and_hospital_mix"] <- "brought_in_hospital_mix"
  cat("変数名: brought_in_and_hospital_mix → brought_in_hospital_mix\n")
}

#============================================================
# 3. 変数型整備
#============================================================

e <- e |>
  mutate(
    id = factor(id),
    sex = factor(sex),
    stroke_type = factor(stroke_type),
    premorbid_manager = factor(premorbid_manager),

    # 曝露変数（5点単位）
    mrci_start_5      = mrci_start / 5,
    delta_mrci_pre30_5 = delta_mrci_pre30 / 5,

    first_error_status = as.integer(first_error_status),
    error_30d          = as.integer(error_30d),

    # 処方安定期間（NA=直前変更なし, 31+含む）
    days_since_change_group = case_when(
      is.na(days_since_last_change)    ~ "No change/unknown",
      days_since_last_change <= 2      ~ "0-2",
      days_since_last_change <= 7      ~ "3-7",
      days_since_last_change <= 14     ~ "8-14",
      days_since_last_change <= 30     ~ "15-30",
      days_since_last_change >= 31     ~ "31+"
    ),

    days_since_change_group = factor(
      days_since_change_group,
      levels = c("31+", "15-30", "8-14", "3-7", "0-2", "No change/unknown")
    )
  )

#============================================================
# 4. データ品質確認
#============================================================

stopifnot(all(e$first_error_status %in% c(0, 1, 2), na.rm = TRUE))
stopifnot(all(e$first_error_time > 0 & e$first_error_time <= 30, na.rm = TRUE))
stopifnot(all(e$self_management_patient_days > 0, na.rm = TRUE))

cat("データ品質確認完了\n")

#============================================================
# 5. 記述統計
#============================================================

cat("\n=== 記述統計 ===\n")

error_summary <- e |>
  summarise(
    analysis_population_n      = n(),
    first_error_n              = sum(first_error_status == 1),
    first_error_percent        = 100 * mean(first_error_status == 1),
    competing_event_n          = sum(first_error_status == 2),
    event_free_30d_n           = sum(first_error_status == 0),
    total_error_events         = sum(error_count, na.rm = TRUE),
    total_patient_days         = sum(self_management_patient_days, na.rm = TRUE),
    errors_per_100_patient_days = 100 * sum(error_count, na.rm = TRUE) /
                                    sum(self_management_patient_days, na.rm = TRUE),
    opportunity_analysis_n     = sum(!is.na(medication_opportunities) &
                                      medication_opportunities > 0),
    total_medication_opportunities = sum(medication_opportunities[
      !is.na(medication_opportunities) & medication_opportunities > 0
    ], na.rm = TRUE),
    errors_per_100_opportunities = 100 * sum(error_count[
      !is.na(medication_opportunities) & medication_opportunities > 0
    ], na.rm = TRUE) / sum(medication_opportunities[
      !is.na(medication_opportunities) & medication_opportunities > 0
    ], na.rm = TRUE)
  )

write_csv(error_summary, file.path(TABLE_DIR, "medication_error_summary.csv"))
print(error_summary)

#============================================================
# 6. 累積発生関数（Aalen-Johansen）
#============================================================

cat("\n=== 累積発生関数 ===\n")

error_cif <- cmprsk::cuminc(
  ftime   = e$first_error_time,
  fstatus = e$first_error_status,
  cencode = 0
)

saveRDS(error_cif, file.path(RESULT_DIR, "medication_error_cif.rds"))

# CIF → data.frame 変換
cuminc_to_df <- function(object) {
  curve_names <- setdiff(names(object), "Tests")
  map_dfr(curve_names, function(curve_name) {
    x <- object[[curve_name]]
    if (is.null(x$time) || is.null(x$est)) return(NULL)
    tibble(
      time     = x$time,
      estimate = x$est,
      variance = x$var,
      cause    = sub(".* ", "", curve_name)
    )
  })
}

error_cif_df <- cuminc_to_df(error_cif) |>
  filter(cause %in% c("1", "2")) |>
  mutate(
    event = factor(cause,
      levels = c("1", "2"),
      labels = c("Patient-initiated medication error",
                 "Discontinuation of self-management, etc.")
    )
  )

#-----------------------------------------------------------
# Figure 5：CIF
#-----------------------------------------------------------

theme_set(theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom"))

figure5 <- ggplot(error_cif_df, aes(x = time, y = estimate, colour = event)) +
  geom_step(linewidth = 1) +
  scale_colour_manual(values = c(
    "Patient-initiated medication error"     = "#C0392B",
    "Discontinuation of self-management, etc." = "#7F8C8D"
  )) +
  scale_y_continuous(labels = label_percent(accuracy = 1)) +
  labs(
    x = "Days after starting full self-management",
    y = "Cumulative incidence",
    colour = "Event",
    title = "Cumulative incidence of medication error after starting self-management",
    subtitle = "Aalen-Johansen estimator accounting for competing events"
  )

ggsave(file.path(FIGURE_DIR, "Figure05_medication_error_CIF.png"),
  figure5, width = 8, height = 6, dpi = 320, bg = "white")
cat("Figure 5 保存完了\n")

#============================================================
# 7. Cause-specific Cox回帰
#============================================================

cat("\n=== Cause-specific Cox ===\n")

base_adjustment <- c("age", "sex", "stroke_type",
                     "fim_motor", "fim_cognitive", "premorbid_manager")

# 検出強度関連変数（存在する場合のみ含める）
detection_variables <- intersect(
  c("confirmation_frequency", "empty_package_check", "remaining_medication_check"),
  names(e)
)

adjustment_variables <- c(base_adjustment, detection_variables)
cat("調整変数:", paste(adjustment_variables, collapse = ", "), "\n")

candidate_exposures <- c(
  "mrci_start_5", "delta_mrci_pre30_5",
  "days_since_change_group", "prescription_changes_pre30",
  "medication_count", "dosing_times",
  "irregular_regimen", "multiple_prescriptions",
  "one_dose_package", "mixed_package", "brought_in_hospital_mix"
)

fit_error_cox <- function(exposure) {
  variables <- unique(c("first_error_time", "first_error_status", "id",
                        exposure, adjustment_variables))
  md <- e |> select(all_of(variables)) |> drop_na()

  if (nrow(md) == 0 || n_distinct(md[[exposure]]) < 2) return(NULL)

  formula <- as.formula(paste0(
    "Surv(first_error_time, first_error_status == 1) ~ ",
    exposure, " + ", paste(adjustment_variables, collapse = " + "),
    " + cluster(id)"
  ))

  fit <- try(coxph(formula, data = md, ties = "efron", x = TRUE, y = TRUE), silent = TRUE)
  if (inherits(fit, "try-error")) return(NULL)

  list(exposure = exposure, fit = fit, data = md)
}

error_cox_models <- compact(map(candidate_exposures, fit_error_cox))

extract_error_cox <- function(object) {
  s <- summary(object$fit)
  target <- grep(paste0("^", object$exposure), rownames(s$coefficients))

  tibble(
    exposure = object$exposure,
    coefficient_name = rownames(s$coefficients)[target],
    hazard_ratio = s$conf.int[target, "exp(coef)"],
    ci_lower  = s$conf.int[target, "lower .95"],
    ci_upper  = s$conf.int[target, "upper .95"],
    robust_se = s$coefficients[target, "robust se"],
    p_value   = s$coefficients[target, "Pr(>|z|)"],
    n         = nrow(object$data),
    events    = sum(object$data$first_error_status == 1)
  )
}

error_cox_table <- map_dfr(error_cox_models, extract_error_cox)

write_csv(error_cox_table, file.path(TABLE_DIR, "Table05_medication_error_associations.csv"))
print(error_cox_table)

#============================================================
# 8. 30日binary estimand（補助解析）
#============================================================

cat("\n=== 30日binary estimand ===\n")

error_binary_vars <- unique(c("error_30d", "id", "mrci_start_5", adjustment_variables))
eb <- e |> select(all_of(error_binary_vars)) |> drop_na()

formula_error_binary <- as.formula(paste0(
  "error_30d ~ mrci_start_5 + ",
  paste(adjustment_variables, collapse = " + ")
))

fit_error_binary <- glm(formula_error_binary, data = eb,
  family = binomial(), x = TRUE, y = TRUE)

V_error_binary <- vcovCL(fit_error_binary, cluster = eb$id, type = "HC0")
error_binary_ct <- coeftest(fit_error_binary, vcov. = V_error_binary)

error_binary_result <- tibble(
  variable    = rownames(error_binary_ct),
  coefficient = error_binary_ct[, 1],
  robust_se    = error_binary_ct[, 2],
  odds_ratio  = exp(error_binary_ct[, 1]),
  ci_lower    = exp(error_binary_ct[, 1] - 1.96 * error_binary_ct[, 2]),
  ci_upper    = exp(error_binary_ct[, 1] + 1.96 * error_binary_ct[, 2]),
  p_value     = error_binary_ct[, 4]
) |>
  filter(variable == "mrci_start_5")

write_csv(error_binary_result, file.path(TABLE_DIR, "error_binary_MRCI_result.csv"))
print(error_binary_result)

#============================================================
# 9. 処方安定期間別記述
#============================================================

cat("\n=== 処方安定期間別エラー率 ===\n")

change_group_table <- e |>
  group_by(days_since_change_group) |>
  summarise(
    n                     = n(),
    error_patients        = sum(first_error_status == 1),
    error_percent         = 100 * mean(first_error_status == 1),
    error_events          = sum(error_count, na.rm = TRUE),
    patient_days          = sum(self_management_patient_days, na.rm = TRUE),
    errors_per_100_patient_days = 100 * sum(error_count, na.rm = TRUE) /
                                    sum(self_management_patient_days, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(change_group_table, file.path(TABLE_DIR, "prescription_stability_error_summary.csv"))
print(change_group_table)

#============================================================
# 10. 反復エラー数：Negative binomial
#============================================================

cat("\n=== 反復エラー数モデル ===\n")

count_data <- e |>
  filter(!is.na(error_count),
         !is.na(self_management_patient_days),
         self_management_patient_days > 0)

count_formula <- error_count ~
  mrci_start_5 + age + sex + stroke_type +
  fim_cognitive + irregular_regimen +
  multiple_prescriptions + one_dose_package + mixed_package +
  offset(log(self_management_patient_days))

fit_poisson <- glm(count_formula, data = count_data,
  family = poisson(link = "log"))

dispersion <- sum(residuals(fit_poisson, type = "pearson")^2) / df.residual(fit_poisson)

if (is.finite(dispersion) && dispersion > 1.5) {
  fit_count <- MASS::glm.nb(count_formula, data = count_data)
  count_model_name <- "Negative binomial"
} else {
  fit_count <- fit_poisson
  count_model_name <- "Poisson"
}

count_result <- broom::tidy(fit_count, conf.int = TRUE, exponentiate = TRUE) |>
  mutate(model = count_model_name, dispersion = dispersion)

write_csv(count_result, file.path(TABLE_DIR, "recurrent_error_count_model.csv"))
print(count_result)

#============================================================
# 11. 解析オブジェクト保存
#============================================================

saveRDS(list(
  error_cif        = error_cif,
  error_cox_models = error_cox_models,
  fit_error_binary = fit_error_binary,
  fit_count        = fit_count
), file.path(RESULT_DIR, "medication_error_manuscript_objects.rds"))

capture.output(sessionInfo(),
  file = file.path(RESULT_DIR, "sessionInfo_medication_error_manuscript.txt"))

cat("\n========================================\n")
cat("服薬エラー解析が完了しました。\n")
cat("表:", TABLE_DIR, "\n")
cat("図:", FIGURE_DIR, "\n")
cat("========================================\n")
