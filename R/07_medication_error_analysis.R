############################################################
# 07_medication_error_analysis.R
#
# 完全内服自己管理開始後の患者起因服薬エラー解析
#
# フォルダ構成:
#
# R/
# ├── 07_medication_error_analysis.R
# ├── data/
# │   └── sample_medication_error.rds
# └── results/
#
# first_error_status:
#   0 = 30日間イベントなし
#   1 = 初回患者起因服薬エラー
#   2 = 完全自己管理中止、退棟等の競合イベント
############################################################

options(
  stringsAsFactors = FALSE,
  warn = 1
)

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

dir.create(DATA_DIRECTORY,    recursive = TRUE, showWarnings = FALSE)
dir.create(RESULTS_DIRECTORY, recursive = TRUE, showWarnings = FALSE)
dir.create(FIGURE_DIRECTORY,  recursive = TRUE, showWarnings = FALSE)

cat(
  "\nRoot:", ROOT,
  "\nData:", DATA_DIRECTORY,
  "\nResults:", RESULTS_DIRECTORY,
  "\n"
)

#============================================================
# 1. 必要パッケージ
#============================================================

required_packages <- c(
  "survival",
  "cmprsk",
  "sandwich",
  "lmtest",
  "MASS"
)

not_installed <- required_packages[
  !required_packages %in%
    rownames(installed.packages())
]

if (length(not_installed) > 0) {

  install.packages(
    not_installed,
    dependencies = TRUE
  )
}

library(survival)
library(cmprsk)
library(sandwich)
library(lmtest)
library(MASS)

#============================================================
# 2. フォルダ設定
#============================================================

project_root <- R_DIRECTORY

data_directory <- DATA_DIRECTORY

results_directory <- RESULTS_DIRECTORY

dir.create(
  data_directory,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  results_directory,
  recursive = TRUE,
  showWarnings = FALSE
)

data_file <- file.path(
  data_directory,
  "sample_medication_error.rds"
)

cat(
  "\n========================================\n"
)

cat(
  "Rフォルダ:\n",
  project_root,
  "\n"
)

cat(
  "データフォルダ:\n",
  data_directory,
  "\n"
)

cat(
  "結果フォルダ:\n",
  results_directory,
  "\n"
)

cat(
  "========================================\n"
)

#============================================================
# 3. データの存在確認
#============================================================

if (!file.exists(data_file)) {

  available_files <- list.files(
    data_directory,
    recursive = TRUE,
    full.names = TRUE
  )

  stop(
    paste0(
      "\n服薬エラーデータが見つかりません。\n\n",
      "探索したファイル:\n",
      data_file,
      "\n\n",
      "dataフォルダ内のファイル:\n",
      if (length(available_files) == 0) {
        "ファイルなし"
      } else {
        paste(
          available_files,
          collapse = "\n"
        )
      },
      "\n\n",
      "05_generate_protocol_sample_data.Rを",
      "先に実行してください。"
    )
  )
}

#============================================================
# 4. データ読み込み
#============================================================

e <- readRDS(
  data_file
)

if (!is.data.frame(e)) {
  stop("読み込んだオブジェクトがdata.frameではありません。")
}

#============================================================
# 変数名の互換性調整
#============================================================

# 合成データ生成コードの版によって、
# brought_in_and_hospital_mixという名称の場合がある。
# 解析コード内ではbrought_in_hospital_mixへ統一する。

if (
  !"brought_in_hospital_mix" %in% names(e) &&
    "brought_in_and_hospital_mix" %in% names(e)
) {

  names(e)[
    names(e) ==
      "brought_in_and_hospital_mix"
  ] <- "brought_in_hospital_mix"

  cat(
    paste0(
      "\n変数名を変更しました:\n",
      "brought_in_and_hospital_mix",
      " → ",
      "brought_in_hospital_mix\n"
    )
  )
}


cat(
  "\n読み込み症例数:",
  nrow(e),
  "\n"
)

#============================================================
# 5. 必須変数の確認
#============================================================

required_variables <- c(
  "id",
  "mrci_start",
  "mrci_start_5",
  "delta_mrci_pre30",
  "delta_mrci_pre30_5",
  "medication_count",
  "dosing_times",
  "daily_opportunities",
  "irregular_regimen",
  "multiple_prescriptions",
  "one_dose_package",
  "mixed_package",
  "brought_in_hospital_mix",
  "prescription_changes_pre30",
  "days_since_last_change",
  "age",
  "sex",
  "stroke_type",
  "fim_motor",
  "fim_cognitive",
  "premorbid_manager",
  "first_error_time",
  "first_error_status",
  "error_30d",
  "error_count",
  "self_management_patient_days",
  "medication_opportunities"
)

missing_variables <- setdiff(
  required_variables,
  names(e)
)

if (length(missing_variables) > 0) {

  stop(
    paste0(
      "\n以下の必須変数がありません:\n",
      paste(
        missing_variables,
        collapse = ", "
      )
    )
  )
}

#============================================================
# 6. 変数型
#============================================================

e$id <- factor(e$id)

e$sex <- factor(
  e$sex,
  levels = c(
    "Male",
    "Female"
  )
)

e$stroke_type <- factor(
  e$stroke_type,
  levels = c(
    "Ischemic",
    "ICH",
    "SAH"
  )
)

e$premorbid_manager <- factor(
  e$premorbid_manager,
  levels = c(
    "Self",
    "Family",
    "Care_staff"
  )
)

binary_variables <- c(
  "irregular_regimen",
  "multiple_prescriptions",
  "one_dose_package",
  "mixed_package",
  "brought_in_hospital_mix",
  "error_30d"
)

for (variable in binary_variables) {

  e[[variable]] <- as.integer(
    e[[variable]]
  )
}

#============================================================
# 7. データ品質確認
#============================================================

if (nrow(e) == 0) {
  stop("服薬エラー解析対象者が0人です。")
}

if (
  anyNA(e$first_error_status)
) {
  warning("first_error_statusに欠測があります。")
}

if (
  any(
    !e$first_error_status %in% c(0, 1, 2),
    na.rm = TRUE
  )
) {
  stop("first_error_statusに0、1、2以外があります。")
}

if (
  any(
    e$first_error_time <= 0 |
      e$first_error_time > 30,
    na.rm = TRUE
  )
) {
  stop("first_error_timeが0以下または30日超です。")
}

if (
  any(
    e$self_management_patient_days <= 0,
    na.rm = TRUE
  )
) {
  stop("self_management_patient_daysに0以下があります。")
}

if (
  any(
    e$error_count < 0,
    na.rm = TRUE
  )
) {
  stop("error_countに負の値があります。")
}

if (
  any(
    e$medication_opportunities < 0,
    na.rm = TRUE
  )
) {
  stop("medication_opportunitiesに負の値があります。")
}

# error_30dとerror_countの整合性
inconsistent_error <- which(
  !is.na(e$error_30d) &
    !is.na(e$error_count) &
    e$error_30d !=
      as.integer(e$error_count > 0)
)

if (length(inconsistent_error) > 0) {

  warning(
    paste0(
      "error_30dとerror_countが一致しない患者が",
      length(inconsistent_error),
      "人います。"
    )
  )
}

# first_error_statusとerror_countの整合性
inconsistent_status <- which(
  !is.na(e$first_error_status) &
    !is.na(e$error_count) &
    (
      (
        e$error_count > 0 &
          e$first_error_status != 1
      ) |
        (
          e$error_count == 0 &
            e$first_error_status == 1
        )
    )
)

if (length(inconsistent_status) > 0) {

  warning(
    paste0(
      "first_error_statusとerror_countが一致しない患者が",
      length(inconsistent_status),
      "人います。"
    )
  )
}

#============================================================
# 8. 服薬機会offset解析対象の作成
#
# medication_opportunitiesが0または欠測でも、
# 解析全体からは除外しない。
# 服薬機会offset解析だけから除外する。
#============================================================

invalid_opportunity <- (
  is.na(e$medication_opportunities) |
    e$medication_opportunities <= 0
)

n_invalid_opportunity <- sum(
  invalid_opportunity
)

cat(
  "\n服薬機会数が0または欠測の患者数:",
  n_invalid_opportunity,
  "\n"
)

if (n_invalid_opportunity > 0) {

  excluded_opportunity_data <- e[
    invalid_opportunity,
    c(
      "id",
      "daily_opportunities",
      "self_management_patient_days",
      "medication_opportunities",
      "error_count"
    ),
    drop = FALSE
  ]

  write.csv(
    excluded_opportunity_data,
    file.path(
      results_directory,
      "excluded_from_opportunity_analysis.csv"
    ),
    row.names = FALSE,
    na = ""
  )
}

e_opportunity <- e[
  !invalid_opportunity,
  ,
  drop = FALSE
]

#============================================================
# 9. 補助関数
#============================================================

safe_rate <- function(
  numerator,
  denominator,
  multiplier = 1
) {

  if (
    is.na(denominator) ||
      denominator <= 0
  ) {
    return(NA_real_)
  }

  multiplier *
    numerator /
    denominator
}

extract_cox_result <- function(
  fit,
  exposure
) {

  s <- summary(fit)

  coefficient_names <- rownames(
    s$coefficients
  )

  target_rows <- grep(
    paste0("^", exposure),
    coefficient_names
  )

  if (length(target_rows) == 0) {
    return(NULL)
  }

  se_column <- if (
    "robust se" %in%
      colnames(s$coefficients)
  ) {
    "robust se"
  } else {
    "se(coef)"
  }

  data.frame(
    exposure = exposure,
    coefficient_name =
      coefficient_names[target_rows],
    coefficient =
      s$coefficients[
        target_rows,
        "coef"
      ],
    hazard_ratio =
      s$conf.int[
        target_rows,
        "exp(coef)"
      ],
    ci_lower =
      s$conf.int[
        target_rows,
        "lower .95"
      ],
    ci_upper =
      s$conf.int[
        target_rows,
        "upper .95"
      ],
    standard_error =
      s$coefficients[
        target_rows,
        se_column
      ],
    p_value =
      s$coefficients[
        target_rows,
        "Pr(>|z|)"
      ],
    row.names = NULL
  )
}

extract_logistic_result <- function(
  fit,
  exposure,
  cluster
) {

  robust_vcov <- vcovCL(
    fit,
    cluster = cluster,
    type = "HC0"
  )

  result <- coeftest(
    fit,
    vcov. = robust_vcov
  )

  coefficient_names <- rownames(result)

  target_rows <- grep(
    paste0("^", exposure),
    coefficient_names
  )

  if (length(target_rows) == 0) {
    return(NULL)
  }

  data.frame(
    exposure = exposure,
    coefficient_name =
      coefficient_names[target_rows],
    coefficient =
      result[target_rows, 1],
    robust_se =
      result[target_rows, 2],
    odds_ratio = exp(
      result[target_rows, 1]
    ),
    ci_lower = exp(
      result[target_rows, 1] -
        1.96 * result[target_rows, 2]
    ),
    ci_upper = exp(
      result[target_rows, 1] +
        1.96 * result[target_rows, 2]
    ),
    p_value =
      result[target_rows, 4],
    row.names = NULL
  )
}

extract_count_result <- function(
  fit,
  model_name
) {

  coefficient_matrix <- summary(
    fit
  )$coefficients

  data.frame(
    variable =
      rownames(coefficient_matrix),
    coefficient =
      coefficient_matrix[, 1],
    standard_error =
      coefficient_matrix[, 2],
    incidence_rate_ratio =
      exp(coefficient_matrix[, 1]),
    ci_lower = exp(
      coefficient_matrix[, 1] -
        1.96 *
          coefficient_matrix[, 2]
    ),
    ci_upper = exp(
      coefficient_matrix[, 1] +
        1.96 *
          coefficient_matrix[, 2]
    ),
    p_value =
      coefficient_matrix[, 4],
    model = model_name,
    row.names = NULL
  )
}

bind_non_null <- function(x) {

  x <- Filter(
    Negate(is.null),
    x
  )

  if (length(x) == 0) {
    return(data.frame())
  }

  do.call(
    rbind,
    x
  )
}

#============================================================
# 10. 記述統計
#============================================================

total_error_events <- sum(
  e$error_count,
  na.rm = TRUE
)

total_patient_days <- sum(
  e$self_management_patient_days,
  na.rm = TRUE
)

total_opportunities <- sum(
  e_opportunity$medication_opportunities,
  na.rm = TRUE
)

error_summary <- data.frame(
  analysis_population_n =
    nrow(e),
  error_experience_n = sum(
    e$error_30d == 1,
    na.rm = TRUE
  ),
  error_experience_percent =
    100 *
      mean(
        e$error_30d == 1,
        na.rm = TRUE
      ),
  first_error_n = sum(
    e$first_error_status == 1,
    na.rm = TRUE
  ),
  competing_event_n = sum(
    e$first_error_status == 2,
    na.rm = TRUE
  ),
  event_free_30d_n = sum(
    e$first_error_status == 0,
    na.rm = TRUE
  ),
  total_error_events =
    total_error_events,
  total_patient_days =
    total_patient_days,
  errors_per_100_patient_days =
    safe_rate(
      total_error_events,
      total_patient_days,
      100
    ),
  opportunity_analysis_n =
    nrow(e_opportunity),
  opportunity_excluded_n =
    n_invalid_opportunity,
  total_medication_opportunities =
    total_opportunities,
  errors_per_100_opportunities =
    safe_rate(
      sum(
        e_opportunity$error_count,
        na.rm = TRUE
      ),
      total_opportunities,
      100
    )
)

write.csv(
  error_summary,
  file.path(
    results_directory,
    "medication_error_summary.csv"
  ),
  row.names = FALSE
)

print(error_summary)

#============================================================
# 11. 累積発生関数
#
# cause 1 = 初回服薬エラー
# cause 2 = 完全自己管理中止等
#============================================================

cif_data <- e[
  complete.cases(
    e[, c(
      "first_error_time",
      "first_error_status"
    )]
  ),
  ,
  drop = FALSE
]

error_cif <- cuminc(
  ftime = cif_data$first_error_time,
  fstatus = cif_data$first_error_status,
  cencode = 0
)

saveRDS(
  error_cif,
  file.path(
    results_directory,
    "medication_error_cif.rds"
  )
)

capture.output(
  print(error_cif),
  file = file.path(
    results_directory,
    "medication_error_cif.txt"
  )
)

#============================================================
# 12. Cause-specific Cox回帰
#============================================================

candidate_exposures <- c(
  "mrci_start_5",
  "delta_mrci_pre30_5",
  "days_since_last_change",
  "prescription_changes_pre30",
  "medication_count",
  "dosing_times",
  "irregular_regimen",
  "multiple_prescriptions",
  "one_dose_package",
  "mixed_package",
  "brought_in_hospital_mix"
)

adjustment_variables <- c(
  "age",
  "sex",
  "stroke_type",
  "fim_motor",
  "fim_cognitive",
  "premorbid_manager"
)

cox_results_list <- list()
cox_fits <- list()

n_first_errors <- sum(
  e$first_error_status == 1,
  na.rm = TRUE
)

cat(
  "\n初回服薬エラー数:",
  n_first_errors,
  "\n"
)

if (n_first_errors > 0) {

  for (exposure in candidate_exposures) {

    analysis_variables <- unique(
      c(
        "first_error_time",
        "first_error_status",
        "id",
        exposure,
        adjustment_variables
      )
    )

    model_data <- e[
      complete.cases(
        e[, analysis_variables, drop = FALSE]
      ),
      ,
      drop = FALSE
    ]

    if (
      nrow(model_data) == 0 ||
        length(
          unique(
            model_data[[exposure]]
          )
        ) < 2
    ) {
      next
    }

    model_formula <- as.formula(
      paste0(
        "Surv(first_error_time, ",
        "first_error_status == 1) ~ ",
        exposure,
        " + ",
        paste(
          adjustment_variables,
          collapse = " + "
        ),
        " + cluster(id)"
      )
    )

    fit <- try(
      coxph(
        model_formula,
        data = model_data,
        ties = "efron",
        x = TRUE,
        y = TRUE
      ),
      silent = TRUE
    )

    if (inherits(fit, "try-error")) {

      warning(
        paste0(
          "Coxモデル推定失敗: ",
          exposure
        )
      )

      next
    }

    cox_fits[[exposure]] <- fit

    cox_results_list[[exposure]] <-
      extract_cox_result(
        fit,
        exposure
      )
  }
}

cox_results <- bind_non_null(
  cox_results_list
)

if (nrow(cox_results) > 0) {

  write.csv(
    cox_results,
    file.path(
      results_directory,
      "medication_error_cause_specific_cox.csv"
    ),
    row.names = FALSE
  )

  print(cox_results)

} else {

  warning(
    "Cause-specific Cox回帰の結果がありません。"
  )
}

#============================================================
# 13. 30日以内エラー経験の補助的解析
#============================================================

logistic_results_list <- list()
logistic_fits <- list()

if (
  length(
    unique(
      e$error_30d[
        !is.na(e$error_30d)
      ]
    )
  ) >= 2
) {

  for (exposure in candidate_exposures) {

    analysis_variables <- unique(
      c(
        "error_30d",
        "id",
        exposure,
        adjustment_variables
      )
    )

    model_data <- e[
      complete.cases(
        e[, analysis_variables, drop = FALSE]
      ),
      ,
      drop = FALSE
    ]

    if (
      nrow(model_data) == 0 ||
        length(
          unique(
            model_data[[exposure]]
          )
        ) < 2
    ) {
      next
    }

    model_formula <- as.formula(
      paste0(
        "error_30d ~ ",
        exposure,
        " + ",
        paste(
          adjustment_variables,
          collapse = " + "
        )
      )
    )

    fit <- try(
      glm(
        model_formula,
        data = model_data,
        family = binomial(link = "logit"),
        x = TRUE,
        y = TRUE
      ),
      silent = TRUE
    )

    if (inherits(fit, "try-error")) {
      next
    }

    logistic_fits[[exposure]] <- fit

    logistic_results_list[[exposure]] <-
      extract_logistic_result(
        fit,
        exposure,
        model_data$id
      )
  }
}

logistic_results <- bind_non_null(
  logistic_results_list
)

if (nrow(logistic_results) > 0) {

  write.csv(
    logistic_results,
    file.path(
      results_directory,
      "medication_error_binary_models.csv"
    ),
    row.names = FALSE
  )
}

#============================================================
# 14. エラー数：自己管理患者日offset
#============================================================

patient_day_variables <- c(
  "error_count",
  "self_management_patient_days",
  "mrci_start_5",
  "age",
  "sex",
  "stroke_type",
  "fim_cognitive",
  "irregular_regimen",
  "multiple_prescriptions",
  "one_dose_package",
  "mixed_package"
)

patient_day_data <- e[
  complete.cases(
    e[, patient_day_variables, drop = FALSE]
  ) &
    e$self_management_patient_days > 0,
  ,
  drop = FALSE
]

patient_day_formula <- error_count ~
  mrci_start_5 +
  age +
  sex +
  stroke_type +
  fim_cognitive +
  irregular_regimen +
  multiple_prescriptions +
  one_dose_package +
  mixed_package +
  offset(
    log(self_management_patient_days)
  )

fit_patient_day_poisson <- glm(
  patient_day_formula,
  data = patient_day_data,
  family = poisson(link = "log")
)

patient_day_dispersion <- sum(
  residuals(
    fit_patient_day_poisson,
    type = "pearson"
  )^2
) / df.residual(
  fit_patient_day_poisson
)

if (
  is.finite(patient_day_dispersion) &&
    patient_day_dispersion > 1.5
) {

  fit_count_patient_day <- MASS::glm.nb(
    patient_day_formula,
    data = patient_day_data
  )

  patient_day_model_type <-
    "Negative binomial"

} else {

  fit_count_patient_day <-
    fit_patient_day_poisson

  patient_day_model_type <-
    "Poisson"
}

patient_day_results <- extract_count_result(
  fit_count_patient_day,
  patient_day_model_type
)

patient_day_results$dispersion <-
  patient_day_dispersion

write.csv(
  patient_day_results,
  file.path(
    results_directory,
    "error_count_patient_day_model.csv"
  ),
  row.names = FALSE
)

#============================================================
# 15. エラー数：服薬機会offset
#============================================================

fit_count_opportunity <- NULL
opportunity_dispersion <- NA_real_
opportunity_model_type <- NA_character_

if (nrow(e_opportunity) > 0) {

  opportunity_variables <- c(
    "error_count",
    "medication_opportunities",
    "mrci_start_5",
    "age",
    "sex",
    "stroke_type",
    "fim_cognitive",
    "irregular_regimen",
    "multiple_prescriptions",
    "one_dose_package",
    "mixed_package"
  )

  opportunity_data <- e_opportunity[
    complete.cases(
      e_opportunity[
        ,
        opportunity_variables,
        drop = FALSE
      ]
    ) &
      e_opportunity$medication_opportunities > 0,
    ,
    drop = FALSE
  ]

  opportunity_formula <- error_count ~
    mrci_start_5 +
    age +
    sex +
    stroke_type +
    fim_cognitive +
    irregular_regimen +
    multiple_prescriptions +
    one_dose_package +
    mixed_package +
    offset(
      log(medication_opportunities)
    )

  fit_opportunity_poisson <- glm(
    opportunity_formula,
    data = opportunity_data,
    family = poisson(link = "log")
  )

  opportunity_dispersion <- sum(
    residuals(
      fit_opportunity_poisson,
      type = "pearson"
    )^2
  ) / df.residual(
    fit_opportunity_poisson
  )

  if (
    is.finite(opportunity_dispersion) &&
      opportunity_dispersion > 1.5
  ) {

    fit_count_opportunity <- MASS::glm.nb(
      opportunity_formula,
      data = opportunity_data
    )

    opportunity_model_type <-
      "Negative binomial"

  } else {

    fit_count_opportunity <-
      fit_opportunity_poisson

    opportunity_model_type <-
      "Poisson"
  }

  opportunity_results <- extract_count_result(
    fit_count_opportunity,
    opportunity_model_type
  )

  opportunity_results$dispersion <-
    opportunity_dispersion

  opportunity_results$
    opportunity_analysis_n <-
    nrow(opportunity_data)

  opportunity_results$
    opportunity_excluded_n <-
    nrow(e) - nrow(opportunity_data)

  write.csv(
    opportunity_results,
    file.path(
      results_directory,
      "error_count_opportunity_model.csv"
    ),
    row.names = FALSE
  )

} else {

  warning(
    paste0(
      "服薬機会数が正の患者がいないため、",
      "服薬機会offset解析を実施しません。"
    )
  )
}

#============================================================
# 16. 最終処方変更からの期間別エラー率
#============================================================

e$days_since_change_group <- cut(
  e$days_since_last_change,
  breaks = c(
    -Inf,
    2,
    7,
    14,
    30,
    Inf
  ),
  labels = c(
    "0-2",
    "3-7",
    "8-14",
    "15-30",
    "31+"
  ),
  right = TRUE
)

change_group_results <- lapply(
  levels(e$days_since_change_group),
  function(group_name) {

    x <- e[
      !is.na(e$days_since_change_group) &
        e$days_since_change_group ==
          group_name,
      ,
      drop = FALSE
    ]

    if (nrow(x) == 0) {

      return(
        data.frame(
          days_since_change_group =
            group_name,
          n = 0,
          error_patients = 0,
          error_patient_percent =
            NA_real_,
          error_events = 0,
          errors_per_100_patient_days =
            NA_real_,
          opportunity_analysis_n = 0,
          errors_per_100_opportunities =
            NA_real_
        )
      )
    }

    x_opportunity <- x[
      !is.na(
        x$medication_opportunities
      ) &
        x$medication_opportunities > 0,
      ,
      drop = FALSE
    ]

    data.frame(
      days_since_change_group =
        group_name,
      n = nrow(x),
      error_patients = sum(
        x$error_30d == 1,
        na.rm = TRUE
      ),
      error_patient_percent =
        100 *
          mean(
            x$error_30d == 1,
            na.rm = TRUE
          ),
      error_events = sum(
        x$error_count,
        na.rm = TRUE
      ),
      errors_per_100_patient_days =
        safe_rate(
          sum(
            x$error_count,
            na.rm = TRUE
          ),
          sum(
            x$self_management_patient_days,
            na.rm = TRUE
          ),
          100
        ),
      opportunity_analysis_n =
        nrow(x_opportunity),
      errors_per_100_opportunities =
        safe_rate(
          sum(
            x_opportunity$error_count,
            na.rm = TRUE
          ),
          sum(
            x_opportunity$
              medication_opportunities,
            na.rm = TRUE
          ),
          100
        )
    )
  }
)

post_change_description <- do.call(
  rbind,
  change_group_results
)

write.csv(
  post_change_description,
  file.path(
    results_directory,
    "error_rate_after_prescription_change.csv"
  ),
  row.names = FALSE
)

print(post_change_description)

#============================================================
# 17. 解析オブジェクト保存
#============================================================

saveRDS(
  list(
    analysis_data = e,
    opportunity_analysis_data =
      e_opportunity,
    error_cif = error_cif,
    cox_fits = cox_fits,
    logistic_fits = logistic_fits,
    fit_count_patient_day =
      fit_count_patient_day,
    fit_count_opportunity =
      fit_count_opportunity,
    patient_day_dispersion =
      patient_day_dispersion,
    opportunity_dispersion =
      opportunity_dispersion,
    opportunity_excluded_n =
      n_invalid_opportunity
  ),
  file.path(
    results_directory,
    "medication_error_analysis_objects.rds"
  )
)

capture.output(
  sessionInfo(),
  file = file.path(
    results_directory,
    "sessionInfo_medication_error.txt"
  )
)

#============================================================
# 18. 完了表示
#============================================================

cat(
  "\n========================================\n"
)

cat(
  "服薬エラー解析が完了しました。\n"
)

cat(
  "解析対象者:",
  nrow(e),
  "\n"
)

cat(
  "初回服薬エラー:",
  sum(
    e$first_error_status == 1,
    na.rm = TRUE
  ),
  "\n"
)

cat(
  "競合イベント:",
  sum(
    e$first_error_status == 2,
    na.rm = TRUE
  ),
  "\n"
)

cat(
  "患者日offsetモデル:",
  patient_day_model_type,
  "\n"
)

cat(
  "服薬機会offsetモデル:",
  ifelse(
    is.na(opportunity_model_type),
    "未実施",
    opportunity_model_type
  ),
  "\n"
)

cat(
  "服薬機会offset解析除外:",
  n_invalid_opportunity,
  "\n"
)

cat(
  "結果保存先:\n",
  results_directory,
  "\n"
)

cat(
  "========================================\n"
)
