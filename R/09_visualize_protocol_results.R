############################################################
# 09_visualize_protocol_results.R
#
# 05-08のデータ・解析結果の図示
#
# 出力:
# results/figures/
#   Figure01_landmark_flow.png
#   Figure02_delta_mrci_distribution.png
#   Figure03_adjusted_probability.png
#   Figure04_model_comparison.png
#   Figure05_landmark_cif.png
#   Figure06_medication_error_cif.png
#   Figure07_error_rate_after_change.png
#   Figure08_medication_error_forest.png
############################################################

options(
  stringsAsFactors = FALSE,
  warn = 1
)

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

#============================================================
# 1. パッケージ
#============================================================

required_packages <- c(
  "ggplot2",
  "cmprsk",
  "sandwich"
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

library(ggplot2)
library(cmprsk)
library(sandwich)

#============================================================
# フォルダ設定
#============================================================

project_root <- R_DIRECTORY

data_directory <- DATA_DIRECTORY

results_directory <- RESULTS_DIRECTORY

figure_directory <- FIGURE_DIRECTORY

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

dir.create(
  figure_directory,
  recursive = TRUE,
  showWarnings = FALSE
)

cat(
  "R directory:",
  project_root,
  "\n"
)

cat(
  "Data directory:",
  data_directory,
  "\n"
)

cat(
  "Results directory:",
  results_directory,
  "\n"
)

cat(
  "Figure directory:",
  figure_directory,
  "\n"
)

#============================================================
# 3. 共通設定
#============================================================

theme_set(
  theme_bw(
    base_size = 12
  ) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(
        face = "bold",
        size = 13
      ),
      plot.subtitle = element_text(
        size = 10,
        colour = "grey30"
      ),
      legend.position = "bottom",
      strip.background = element_rect(
        fill = "grey95"
      )
    )
)

percent_label <- function(
  x,
  digits = 0
) {
  paste0(
    formatC(
      100 * x,
      format = "f",
      digits = digits
    ),
    "%"
  )
}

save_figure <- function(
  plot,
  filename,
  width = 8,
  height = 6
) {

  ggsave(
    filename = file.path(
      figure_directory,
      filename
    ),
    plot = plot,
    width = width,
    height = height,
    dpi = 320,
    bg = "white"
  )
}

#============================================================
# 4. データ読み込み
#============================================================

landmark_file <- file.path(
  data_directory,
  "sample_landmark_long.rds"
)

error_file <- file.path(
  data_directory,
  "sample_medication_error.rds"
)

primary_object_file <- file.path(
  results_directory,
  "primary_analysis_objects.rds"
)

required_files <- c(
  landmark_file,
  error_file,
  primary_object_file
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0) {

  stop(
    paste0(
      "必要なファイルがありません。\n",
      paste(
        missing_files,
        collapse = "\n"
      ),
      "\n\n先にR/08_run_protocol_analysis.Rを実行してください。"
    )
  )
}

d <- readRDS(
  landmark_file
)

e <- readRDS(
  error_file
)

primary_objects <- readRDS(
  primary_object_file
)

#============================================================
# 5. 変数型
#============================================================

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

d$calendar_year <- factor(
  d$calendar_year
)

d$premorbid_manager <- factor(
  d$premorbid_manager,
  levels = c(
    "Self",
    "Family",
    "Care_staff"
  )
)

d$family_support <- factor(
  d$family_support,
  levels = c("No", "Yes")
)

d$planned_destination <- factor(
  d$planned_destination,
  levels = c(
    "Home",
    "Facility",
    "Undecided"
  )
)

d$previous_management_level <- factor(
  d$previous_management_level,
  levels = c(1, 2, 3),
  ordered = FALSE
)

e$id <- factor(e$id)

#============================================================
# Figure 1
# ランドマークごとのリスク集合と30日転帰
#============================================================

d$outcome_30d <- factor(
  d$status,
  levels = c(0, 2, 1),
  labels = c(
    "No event by 30 days",
    "Competing event",
    "Self-management"
  )
)

landmark_flow_plot <- as.data.frame(
  table(
    landmark = d$landmark,
    outcome = d$outcome_30d
  )
)

names(landmark_flow_plot)[3] <- "n"

p1 <- ggplot(
  landmark_flow_plot,
  aes(
    x = landmark,
    y = n,
    fill = outcome
  )
) +
  geom_col(
    width = 0.72
  ) +
  geom_text(
    aes(label = n),
    position = position_stack(
      vjust = 0.5
    ),
    size = 3.4,
    colour = "white"
  ) +
  scale_fill_manual(
    values = c(
      "No event by 30 days" = "#7F8C8D",
      "Competing event" = "#E67E22",
      "Self-management" = "#2874A6"
    )
  ) +
  labs(
    title = "Sequential landmark risk sets and 30-day outcomes",
    subtitle = paste0(
      "Each patient may contribute to more than one landmark; ",
      "therefore bars represent landmark rows, not unique patients."
    ),
    x = "Landmark day",
    y = "Number of landmark rows",
    fill = "30-day outcome"
  )

save_figure(
  p1,
  "Figure01_landmark_flow.png",
  width = 8,
  height = 6
)

#============================================================
# Figure 2
# MRCI-J変化量の分布
#
# delta_mrci = previous_mrci - current_mrci
# 正値ほどMRCI-J低下＝処方簡素化
#============================================================

p2 <- ggplot(
  d,
  aes(
    x = landmark,
    y = delta_mrci,
    fill = landmark
  )
) +
  geom_violin(
    trim = FALSE,
    alpha = 0.60,
    colour = NA
  ) +
  geom_boxplot(
    width = 0.16,
    outlier.shape = NA,
    fill = "white",
    linewidth = 0.4
  ) +
  geom_hline(
    yintercept = 0,
    linetype = 2,
    colour = "grey35"
  ) +
  scale_fill_brewer(
    palette = "Blues",
    guide = "none"
  ) +
  labs(
    title = "Distribution of preceding MRCI-J change",
    subtitle = paste0(
      "Positive values indicate decreased MRCI-J ",
      "(regimen simplification)."
    ),
    x = "Landmark day",
    y = "MRCI-J change: previous minus current"
  )

save_figure(
  p2,
  "Figure02_delta_mrci_distribution.png",
  width = 8,
  height = 6
)

#============================================================
# Figure 3
# 調整済み30日自己管理移行確率
#
# 各delta5値について、その他の変数は観測値のまま
# 全ランドマーク行で予測し、平均する。
#
# CIは患者cluster-robust covarianceを用いたdelta method。
#============================================================

fit_main <- primary_objects$fit_main
main_vcov <- primary_objects$main_vcov

standardized_curve <- function(
  fit,
  data,
  vcov_matrix,
  values
) {

  beta_names <- names(
    coef(fit)
  )

  output <- vector(
    "list",
    length(values)
  )

  for (i in seq_along(values)) {

    newdata <- data
    newdata$delta5 <- values[i]

    predicted <- predict(
      fit,
      newdata = newdata,
      type = "response"
    )

    X <- model.matrix(
      delete.response(
        terms(fit)
      ),
      data = newdata,
      contrasts.arg = fit$contrasts,
      xlev = fit$xlevels
    )

    X <- X[
      ,
      beta_names,
      drop = FALSE
    ]

    gradient <- colMeans(
      X *
        as.numeric(
          predicted *
            (1 - predicted)
        )
    )

    standard_error <- sqrt(
      as.numeric(
        t(gradient) %*%
          vcov_matrix %*%
          gradient
      )
    )

    estimate <- mean(
      predicted,
      na.rm = TRUE
    )

    output[[i]] <- data.frame(
      delta5 = values[i],
      delta_mrci = 5 * values[i],
      probability = estimate,
      lower = max(
        0,
        estimate - 1.96 * standard_error
      ),
      upper = min(
        1,
        estimate + 1.96 * standard_error
      )
    )
  }

  do.call(
    rbind,
    output
  )
}

delta_limits <- quantile(
  d$delta5,
  probs = c(0.01, 0.99),
  na.rm = TRUE
)

delta_grid <- seq(
  delta_limits[1],
  delta_limits[2],
  length.out = 100
)

adjusted_curve <- standardized_curve(
  fit = fit_main,
  data = d,
  vcov_matrix = main_vcov,
  values = delta_grid
)

write.csv(
  adjusted_curve,
  file.path(
    results_directory,
    "adjusted_probability_curve.csv"
  ),
  row.names = FALSE
)

p3 <- ggplot(
  adjusted_curve,
  aes(
    x = delta_mrci,
    y = probability
  )
) +
  geom_ribbon(
    aes(
      ymin = lower,
      ymax = upper
    ),
    fill = "#5DADE2",
    alpha = 0.25
  ) +
  geom_line(
    colour = "#1B4F72",
    linewidth = 1.1
  ) +
  geom_vline(
    xintercept = 0,
    linetype = 2,
    colour = "grey35"
  ) +
  scale_y_continuous(
    labels = function(x) {
      percent_label(
        x,
        digits = 0
      )
    },
    limits = c(
      0,
      max(
        adjusted_curve$upper,
        na.rm = TRUE
      ) * 1.05
    )
  ) +
  labs(
    title = "Adjusted 30-day probability of full self-management",
    subtitle = paste0(
      "Standardized predictions from the pooled logistic model; ",
      "shaded area is a model-based 95% CI."
    ),
    x = paste0(
      "MRCI-J decrease during the preceding period ",
      "(positive = simplification)"
    ),
    y = "Adjusted 30-day probability"
  )

save_figure(
  p3,
  "Figure03_adjusted_probability.png",
  width = 8,
  height = 6
)

#============================================================
# Figure 4
# Model 1-3におけるMRCI-J 5点低下のOR
#============================================================

model_files <- c(
  "Model 1: basic" =
    "model1_basic.csv",
  "Model 2: patient state" =
    "model2_patient_state.csv",
  "Model 3: clinical judgement" =
    "model3_clinical_judgement.csv"
)

model_comparison <- do.call(
  rbind,
  lapply(
    names(model_files),
    function(model_name) {

      file <- file.path(
        results_directory,
        model_files[[model_name]]
      )

      if (!file.exists(file)) {
        return(NULL)
      }

      x <- read.csv(
        file,
        check.names = FALSE
      )

      x <- x[
        x$variable == "delta5",
        ,
        drop = FALSE
      ]

      if (nrow(x) == 0) {
        return(NULL)
      }

      data.frame(
        model = model_name,
        odds_ratio = x$odds_ratio,
        ci_lower = x$ci_lower,
        ci_upper = x$ci_upper
      )
    }
  )
)

model_comparison$model <- factor(
  model_comparison$model,
  levels = rev(
    names(model_files)
  )
)

p4 <- ggplot(
  model_comparison,
  aes(
    x = odds_ratio,
    y = model
  )
) +
  geom_vline(
    xintercept = 1,
    linetype = 2,
    colour = "grey45"
  ) +
  geom_errorbarh(
    aes(
      xmin = ci_lower,
      xmax = ci_upper
    ),
    height = 0.15,
    linewidth = 0.7
  ) +
  geom_point(
    size = 3,
    colour = "#1B4F72"
  ) +
  scale_x_log10() +
  labs(
    title = "Association of a 5-point MRCI-J decrease with self-management",
    subtitle = "Odds ratios with patient-cluster robust 95% confidence intervals",
    x = "Odds ratio per 5-point MRCI-J decrease (log scale)",
    y = NULL
  )

save_figure(
  p4,
  "Figure04_model_comparison.png",
  width = 8,
  height = 5
)

#============================================================
# 補助関数：cumincオブジェクトをdata.frameへ変換
#============================================================

cuminc_to_data_frame <- function(
  object,
  group_label = NULL
) {

  curve_names <- setdiff(
    names(object),
    "Tests"
  )

  output <- lapply(
    curve_names,
    function(curve_name) {

      curve <- object[[curve_name]]

      if (
        is.null(curve$time) ||
          is.null(curve$est)
      ) {
        return(NULL)
      }

      cause_number <- sub(
        ".* ",
        "",
        curve_name
      )

      data.frame(
        time = curve$time,
        estimate = curve$est,
        variance = if (
          is.null(curve$var)
        ) {
          NA_real_
        } else {
          curve$var
        },
        cause = cause_number,
        group = if (
          is.null(group_label)
        ) {
          curve_name
        } else {
          group_label
        }
      )
    }
  )

  output <- Filter(
    Negate(is.null),
    output
  )

  do.call(
    rbind,
    output
  )
}

#============================================================
# Figure 5
# ランドマーク別Aalen-Johansen累積発生関数
#============================================================

landmark_cif_file <- file.path(
  results_directory,
  "cumulative_incidence_by_landmark.rds"
)

landmark_cif <- readRDS(
  landmark_cif_file
)

landmark_cif_data <- do.call(
  rbind,
  lapply(
    names(landmark_cif),
    function(landmark_name) {

      cuminc_to_data_frame(
        landmark_cif[[landmark_name]],
        group_label = paste0(
          "Day ",
          landmark_name
        )
      )
    }
  )
)

landmark_cif_data <- landmark_cif_data[
  landmark_cif_data$cause %in%
    c("1", "2"),
  ,
  drop = FALSE
]

landmark_cif_data$event <- factor(
  landmark_cif_data$cause,
  levels = c("1", "2"),
  labels = c(
    "Full self-management",
    "Competing event"
  )
)

p5 <- ggplot(
  landmark_cif_data,
  aes(
    x = time,
    y = estimate,
    colour = group
  )
) +
  geom_step(
    linewidth = 0.9
  ) +
  facet_wrap(
    ~ event,
    ncol = 1,
    scales = "free_y"
  ) +
  scale_y_continuous(
    labels = function(x) {
      percent_label(
        x,
        digits = 0
      )
    }
  ) +
  scale_colour_brewer(
    palette = "Dark2"
  ) +
  labs(
    title = "Unadjusted cumulative incidence by landmark",
    subtitle = "Aalen–Johansen cumulative incidence estimates",
    x = "Days after landmark",
    y = "Cumulative incidence",
    colour = "Landmark"
  )

save_figure(
  p5,
  "Figure05_landmark_cif.png",
  width = 8,
  height = 8
)

#============================================================
# Figure 6
# 完全自己管理開始後の初回服薬エラーCIF
#============================================================

error_cif <- cmprsk::cuminc(
  ftime = e$first_error_time,
  fstatus = e$first_error_status,
  cencode = 0
)

error_cif_data <- cuminc_to_data_frame(
  error_cif,
  group_label = "All patients"
)

error_cif_data <- error_cif_data[
  error_cif_data$cause %in%
    c("1", "2"),
  ,
  drop = FALSE
]

error_cif_data$event <- factor(
  error_cif_data$cause,
  levels = c("1", "2"),
  labels = c(
    "First medication error",
    "Competing event"
  )
)

p6 <- ggplot(
  error_cif_data,
  aes(
    x = time,
    y = estimate,
    colour = event
  )
) +
  geom_step(
    linewidth = 1.05
  ) +
  scale_colour_manual(
    values = c(
      "First medication error" = "#C0392B",
      "Competing event" = "#7F8C8D"
    )
  ) +
  scale_y_continuous(
    labels = function(x) {
      percent_label(
        x,
        digits = 0
      )
    }
  ) +
  labs(
    title = "Cumulative incidence after starting full self-management",
    subtitle = "Competing event: discontinuation of self-management, discharge, etc.",
    x = "Days after starting full self-management",
    y = "Cumulative incidence",
    colour = "Event"
  )

save_figure(
  p6,
  "Figure06_medication_error_cif.png",
  width = 8,
  height = 6
)

#============================================================
# Figure 7
# 最終処方変更からの日数別エラー率
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

change_levels <- levels(
  e$days_since_change_group
)

error_rate_data <- do.call(
  rbind,
  lapply(
    change_levels,
    function(group_name) {

      x <- e[
        !is.na(e$days_since_change_group) &
          e$days_since_change_group ==
            group_name,
        ,
        drop = FALSE
      ]

      denominator <- sum(
        x$self_management_patient_days,
        na.rm = TRUE
      )

      data.frame(
        days_since_change_group =
          group_name,
        n = nrow(x),
        error_events = sum(
          x$error_count,
          na.rm = TRUE
        ),
        patient_days = denominator,
        errors_per_100_patient_days =
          ifelse(
            denominator > 0,
            100 *
              sum(
                x$error_count,
                na.rm = TRUE
              ) /
              denominator,
            NA_real_
          )
      )
    }
  )
)

error_rate_data$days_since_change_group <- factor(
  error_rate_data$days_since_change_group,
  levels = change_levels
)

p7 <- ggplot(
  error_rate_data,
  aes(
    x = days_since_change_group,
    y = errors_per_100_patient_days
  )
) +
  geom_col(
    fill = "#C0392B",
    width = 0.68
  ) +
  geom_text(
    aes(
      label = paste0(
        "n=",
        n
      )
    ),
    vjust = -0.4,
    size = 3.4
  ) +
  expand_limits(
    y = max(
      error_rate_data$
        errors_per_100_patient_days,
      na.rm = TRUE
    ) * 1.15
  ) +
  labs(
    title = "Medication-error rate by time since prescription change",
    subtitle = "Crude rates; n indicates the number of patients in each category",
    x = "Days since last prescription change",
    y = "Medication errors per 100 self-management patient-days"
  )

save_figure(
  p7,
  "Figure07_error_rate_after_change.png",
  width = 8,
  height = 6
)

#============================================================
# Figure 8
# 服薬エラーCause-specific Coxモデル
#============================================================

error_cox_file <- file.path(
  results_directory,
  "medication_error_cause_specific_cox.csv"
)

if (file.exists(error_cox_file)) {

  error_cox <- read.csv(
    error_cox_file,
    check.names = FALSE
  )

  exposure_labels <- c(
    mrci_start_5 =
      "MRCI-J at start, per 5 points",
    delta_mrci_pre30_5 =
      "Preceding MRCI-J decrease, per 5 points",
    days_since_last_change =
      "Days since last prescription change",
    prescription_changes_pre30 =
      "Number of prescription changes",
    medication_count =
      "Number of medications",
    dosing_times =
      "Number of dosing times",
    irregular_regimen =
      "Irregular regimen",
    multiple_prescriptions =
      "Multiple prescriptions",
    one_dose_package =
      "One-dose packaging",
    mixed_package =
      "Mixed packaging",
    brought_in_hospital_mix =
      "Mixed brought-in/hospital medications"
  )

  error_cox$label <- unname(
    exposure_labels[
      error_cox$exposure
    ]
  )

  missing_label <- is.na(
    error_cox$label
  )

  error_cox$label[missing_label] <-
    error_cox$exposure[missing_label]

  error_cox$label <- factor(
    error_cox$label,
    levels = rev(
      error_cox$label
    )
  )

  p8 <- ggplot(
    error_cox,
    aes(
      x = hazard_ratio,
      y = label
    )
  ) +
    geom_vline(
      xintercept = 1,
      linetype = 2,
      colour = "grey45"
    ) +
    geom_errorbarh(
      aes(
        xmin = ci_lower,
        xmax = ci_upper
      ),
      height = 0.15,
      linewidth = 0.7
    ) +
    geom_point(
      size = 2.8,
      colour = "#922B21"
    ) +
    scale_x_log10() +
    labs(
      title = "Candidate predictors of first medication error",
      subtitle = paste0(
        "Separate adjusted cause-specific Cox models; ",
        "competing events are treated as censoring."
      ),
      x = "Cause-specific hazard ratio (log scale)",
      y = NULL
    )

  save_figure(
    p8,
    "Figure08_medication_error_forest.png",
    width = 9,
    height = 7
  )

} else {

  warning(
    paste0(
      "服薬エラーCox結果がないためFigure 8を作成しません:\n",
      error_cox_file
    )
  )
}

#============================================================
# 解析図オブジェクト保存
#============================================================

figure_objects <- list(
  Figure01_landmark_flow = p1,
  Figure02_delta_mrci_distribution = p2,
  Figure03_adjusted_probability = p3,
  Figure04_model_comparison = p4,
  Figure05_landmark_cif = p5,
  Figure06_medication_error_cif = p6,
  Figure07_error_rate_after_change = p7
)

if (exists("p8")) {
  figure_objects$
    Figure08_medication_error_forest <- p8
}

saveRDS(
  figure_objects,
  file.path(
    results_directory,
    "figure_objects.rds"
  )
)

capture.output(
  sessionInfo(),
  file = file.path(
    results_directory,
    "sessionInfo_visualization.txt"
  )
)

cat(
  "\n========================================\n"
)

cat(
  "図の作成が完了しました。\n"
)

cat(
  "保存先:\n",
  figure_directory,
  "\n"
)

cat(
  "========================================\n"
)
