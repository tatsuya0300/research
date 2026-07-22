############################################################
# 02_main_analysis.R
# 主要解析：修正版
############################################################

#-----------------------------------------------------------
# 必要パッケージ
#-----------------------------------------------------------

required_packages <- c(
  "survival",
  "cmprsk",
  "riskRegression",
  "prodlim",
  "splines",
  "ggplot2",
  "scales"
)

not_installed <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if (length(not_installed) > 0) {
  install.packages(not_installed)
}

library(survival)
library(cmprsk)
library(riskRegression)
library(prodlim)
library(splines)
library(ggplot2)

#-----------------------------------------------------------
# 補助関数
#-----------------------------------------------------------

# Cox回帰結果をデータフレーム化
extract_cox_results <- function(fit) {

  s <- summary(fit)

  data.frame(
    variable = rownames(s$coefficients),
    HR = exp(s$coefficients[, "coef"]),
    CI_lower = s$conf.int[, "lower .95"],
    CI_upper = s$conf.int[, "upper .95"],
    p_value = s$coefficients[, "Pr(>|z|)"],
    row.names = NULL
  )
}

# cumincオブジェクトをデータフレーム化
cuminc_to_data <- function(cuminc_object) {

  curve_names <- names(cuminc_object)[
    vapply(
      cuminc_object,
      function(x) {
        is.list(x) &&
          all(c("time", "est", "var") %in% names(x))
      },
      logical(1)
    )
  ]

  if (length(curve_names) == 0) {
    stop("cumincオブジェクトから曲線を抽出できませんでした。")
  }

  result <- do.call(
    rbind,
    lapply(
      curve_names,
      function(curve_name) {

        curve <- cuminc_object[[curve_name]]

        # 曲線名の末尾をcauseコードとして取得
        cause_code <- sub(
          pattern = "^.* ",
          replacement = "",
          x = curve_name
        )

        # 曲線名からcauseコードを除いた部分
        group_name <- sub(
          pattern = " [^ ]+$",
          replacement = "",
          x = curve_name
        )

        data.frame(
          curve_name = curve_name,
          group = group_name,
          cause_code = cause_code,
          time = curve$time,
          estimate = curve$est,
          variance = curve$var,
          stringsAsFactors = FALSE
        )
      }
    )
  )

  result$standard_error <- sqrt(
    pmax(result$variance, 0)
  )

  result$lower <- pmax(
    0,
    result$estimate -
      1.96 * result$standard_error
  )

  result$upper <- pmin(
    1,
    result$estimate +
      1.96 * result$standard_error
  )

  rownames(result) <- NULL

  result
}

# Y軸上限を設定
calculate_y_upper <- function(x) {

  maximum_value <- max(
    x,
    na.rm = TRUE
  )

  y_upper <- ceiling(
    (maximum_value + 0.05) * 10
  ) / 10

  y_upper <- min(
    max(y_upper, 0.2),
    1
  )

  y_upper
}

#-----------------------------------------------------------
# データ読み込み
#-----------------------------------------------------------

d <- read.csv(
  "sample_stroke_mrci_landmark.csv",
  stringsAsFactors = FALSE
)

#-----------------------------------------------------------
# 変数型の設定
#-----------------------------------------------------------

d$sex <- factor(
  d$sex,
  levels = c("Male", "Female")
)

d$stroke_type <- factor(
  d$stroke_type,
  levels = c("Ischemic", "ICH", "SAH")
)

d$premorbid_manager <- factor(
  d$premorbid_manager,
  levels = c(
    "Self",
    "Family",
    "Care_staff",
    "No_medication"
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

d$clinical_judgement14 <- factor(
  d$clinical_judgement14,
  levels = c(
    "Currently_difficult",
    "Reassess_or_conditional",
    "Suitable"
  ),
  ordered = TRUE
)

d$eligible30 <- as.logical(
  d$eligible30
)

# 主要曝露：5点単位
d$delta5 <- d$delta_mrci14 / 5

# 14日目MRCI-J：5点単位
d$mrci14_5 <- d$mrci14 / 5

#-----------------------------------------------------------
# データ品質確認
#-----------------------------------------------------------

stopifnot(
  all(d$fu_days > 0, na.rm = TRUE)
)

stopifnot(
  all(d$status %in% c(0, 1, 2))
)

stopifnot(
  all(
    abs(
      d$delta_mrci14 -
        (d$mrci0 - d$mrci14)
    ) < 0.001
  )
)

cat("\n解析対象者数:", nrow(d), "\n")
cat(
  "自己管理獲得数:",
  sum(d$status == 1, na.rm = TRUE),
  "\n"
)
cat(
  "競合イベント数:",
  sum(d$status == 2, na.rm = TRUE),
  "\n"
)
cat(
  "行政的打切り数:",
  sum(d$status == 0, na.rm = TRUE),
  "\n"
)

#-----------------------------------------------------------
# 記述統計
#-----------------------------------------------------------

continuous_vars <- c(
  "age",
  "onset_to_admission",
  "mrci0",
  "mrci14",
  "delta_mrci14",
  "fim_motor14",
  "fim_cognitive14",
  "fu_days"
)

description <- lapply(
  d[continuous_vars],
  function(x) {
    c(
      n = sum(!is.na(x)),
      mean = mean(x, na.rm = TRUE),
      sd = sd(x, na.rm = TRUE),
      median = median(x, na.rm = TRUE),
      q1 = unname(
        quantile(x, 0.25, na.rm = TRUE)
      ),
      q3 = unname(
        quantile(x, 0.75, na.rm = TRUE)
      ),
      min = min(x, na.rm = TRUE),
      max = max(x, na.rm = TRUE)
    )
  }
)

description <- do.call(
  rbind,
  description
)

print(
  round(description, 2)
)

write.csv(
  description,
  file = "result_descriptive_statistics.csv",
  row.names = TRUE
)

cat("\n脳卒中病型\n")
print(
  table(d$stroke_type, useNA = "ifany")
)

cat("\n発症前服薬管理者\n")
print(
  table(d$premorbid_manager, useNA = "ifany")
)

cat("\n予定退院先\n")
print(
  table(d$planned_destination, useNA = "ifany")
)

cat("\n14日目自己管理判断\n")
print(
  table(d$clinical_judgement14, useNA = "ifany")
)

#-----------------------------------------------------------
# 主要解析：
# cause-specific Cox比例ハザード回帰
#-----------------------------------------------------------

fit_cs_main <- coxph(
  Surv(fu_days, status == 1) ~
    delta5 +
    age +
    sex +
    stroke_type +
    onset_to_admission +
    mrci0 +
    fim_motor14 +
    fim_cognitive14 +
    premorbid_manager +
    family_support +
    planned_destination,
  data = d,
  ties = "efron",
  x = TRUE,
  y = TRUE
)

print(
  summary(fit_cs_main)
)

cox_results <- extract_cox_results(
  fit_cs_main
)

print(cox_results)

write.csv(
  cox_results,
  file = "result_cause_specific_cox.csv",
  row.names = FALSE
)

primary_result <- subset(
  cox_results,
  variable == "delta5"
)

cat(
  "\nMRCI-J 5点単純化当たりの主要結果\n"
)

print(primary_result)

#-----------------------------------------------------------
# 比例ハザード仮定
#-----------------------------------------------------------

ph_test <- cox.zph(
  fit_cs_main,
  transform = "km"
)

print(ph_test)

ph_results <- data.frame(
  variable = rownames(ph_test$table),
  ph_test$table,
  row.names = NULL,
  check.names = FALSE
)

write.csv(
  ph_results,
  file = "result_proportional_hazards_test.csv",
  row.names = FALSE
)

png(
  filename = "plot_schoenfeld_delta5.png",
  width = 1000,
  height = 750,
  res = 120
)

par(
  mar = c(5.5, 5.5, 5, 2) + 0.1
)

plot(
  ph_test["delta5"],
  main = "",
  xlab = "Time",
  ylab = "Scaled Schoenfeld residual"
)

title(
  main = "Schoenfeld residuals: MRCI-J simplification",
  line = 2,
  font.main = 2,
  cex.main = 1.25
)

abline(
  h = 0,
  lty = 2,
  col = "gray40"
)

dev.off()

#-----------------------------------------------------------
# 14日目自己管理判断を追加した補助モデル
#-----------------------------------------------------------

d$clinical_judgement14_nominal <- factor(
  as.character(d$clinical_judgement14),
  levels = c(
    "Currently_difficult",
    "Reassess_or_conditional",
    "Suitable"
  )
)

fit_cs_judgement <- coxph(
  Surv(fu_days, status == 1) ~
    delta5 +
    age +
    sex +
    stroke_type +
    onset_to_admission +
    mrci0 +
    fim_motor14 +
    fim_cognitive14 +
    premorbid_manager +
    family_support +
    planned_destination +
    clinical_judgement14_nominal,
  data = d,
  ties = "efron",
  x = TRUE,
  y = TRUE
)

print(
  summary(fit_cs_judgement)
)

judgement_results <- extract_cox_results(
  fit_cs_judgement
)

write.csv(
  judgement_results,
  file = "result_cause_specific_cox_with_judgement.csv",
  row.names = FALSE
)

#-----------------------------------------------------------
# 代替モデル：
# 14日目MRCI-Jを曝露とし、入棟時MRCI-Jで調整
#-----------------------------------------------------------

fit_mrci14 <- coxph(
  Surv(fu_days, status == 1) ~
    mrci14_5 +
    mrci0 +
    age +
    sex +
    stroke_type +
    onset_to_admission +
    fim_motor14 +
    fim_cognitive14 +
    premorbid_manager +
    family_support +
    planned_destination,
  data = d,
  ties = "efron",
  x = TRUE,
  y = TRUE
)

print(
  summary(fit_mrci14)
)

mrci14_results <- extract_cox_results(
  fit_mrci14
)

write.csv(
  mrci14_results,
  file = "result_cause_specific_cox_mrci14.csv",
  row.names = FALSE
)

#-----------------------------------------------------------
# 全体の累積発生関数
#-----------------------------------------------------------

cif_overall <- cuminc(
  ftime = d$fu_days,
  fstatus = d$status,
  cencode = 0
)

print(cif_overall)

cif_overall_data <- cuminc_to_data(
  cif_overall
)

cif_overall_data$cause <- factor(
  cif_overall_data$cause_code,
  levels = c("1", "2"),
  labels = c(
    "Medication self-management",
    "Competing event"
  )
)

write.csv(
  cif_overall_data,
  file = "result_cumulative_incidence_overall.csv",
  row.names = FALSE
)

maximum_followup_overall <- ceiling(
  max(d$fu_days, na.rm = TRUE) / 30
) * 30

y_upper_overall <- calculate_y_upper(
  cif_overall_data$estimate
)

p_cif_overall <- ggplot(
  cif_overall_data,
  aes(
    x = time,
    y = estimate,
    colour = cause,
    linetype = cause,
    group = cause
  )
) +
  geom_step(
    linewidth = 1.2,
    direction = "hv"
  ) +
  scale_colour_manual(
    name = NULL,
    values = c(
      "Medication self-management" = "#0072B2",
      "Competing event" = "#D55E00"
    )
  ) +
  scale_linetype_manual(
    name = NULL,
    values = c(
      "Medication self-management" = "solid",
      "Competing event" = "longdash"
    )
  ) +
  scale_x_continuous(
    name = "Days after the day-14 landmark",
    breaks = seq(
      0,
      maximum_followup_overall,
      by = 30
    ),
    expand = expansion(
      mult = c(0, 0.01)
    )
  ) +
  scale_y_continuous(
    name = "Cumulative incidence",
    labels = scales::label_percent(
      accuracy = 1
    ),
    breaks = scales::pretty_breaks(
      n = 6
    ),
    expand = expansion(
      mult = c(0, 0.02)
    )
  ) +
  coord_cartesian(
    xlim = c(0, maximum_followup_overall),
    ylim = c(0, y_upper_overall),
    clip = "off"
  ) +
  labs(
    title = "Cumulative incidence after the day-14 landmark",
    subtitle = paste0(
      "Medication self-management and competing events"
    ),
    caption = paste0(
      "Cumulative incidence functions were estimated ",
      "while accounting for competing risks."
    )
  ) +
  theme_classic(
    base_size = 13
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 15
    ),
    plot.subtitle = element_text(
      size = 11,
      margin = margin(b = 10)
    ),
    axis.title = element_text(
      face = "bold"
    ),
    axis.text = element_text(
      colour = "black"
    ),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(
      size = 11
    ),
    legend.key.width = grid::unit(
      2,
      "cm"
    ),
    plot.caption = element_text(
      hjust = 0,
      size = 9,
      colour = "gray30",
      margin = margin(t = 10)
    ),
    plot.margin = margin(
      t = 10,
      r = 15,
      b = 10,
      l = 10
    )
  ) +
  guides(
    colour = guide_legend(
      nrow = 1,
      byrow = TRUE
    ),
    linetype = guide_legend(
      nrow = 1,
      byrow = TRUE
    )
  )

print(p_cif_overall)

ggsave(
  filename = "plot_cumulative_incidence_overall.png",
  plot = p_cif_overall,
  width = 9,
  height = 7,
  units = "in",
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = "plot_cumulative_incidence_overall.pdf",
  plot = p_cif_overall,
  width = 9,
  height = 7,
  units = "in",
  bg = "white"
)

#-----------------------------------------------------------
# 記述目的のMRCI-J変化群
#
# <= 0  ：単純化なし、または複雑化
# >0～5 ：0点超5点以下の単純化
# >5    ：5点超の単純化
#-----------------------------------------------------------

d$delta_group <- cut(
  d$delta_mrci14,
  breaks = c(
    -Inf,
    0,
    5,
    Inf
  ),
  labels = c(
    "No simplification / increase",
    ">0 to 5 points",
    ">5 points"
  ),
  right = TRUE,
  include.lowest = TRUE
)

d$delta_group <- droplevels(
  d$delta_group
)

cat(
  "\nMRCI-J単純化量カテゴリー\n"
)

print(
  table(
    d$delta_group,
    useNA = "ifany"
  )
)

#-----------------------------------------------------------
# 群別累積発生関数
#-----------------------------------------------------------

cif_group <- cuminc(
  ftime = d$fu_days,
  fstatus = d$status,
  group = d$delta_group,
  cencode = 0
)

print(cif_group)

cif_group_data_all <- cuminc_to_data(
  cif_group
)

# 自己管理獲得（cause 1）のみ抽出
cif_group_data <- subset(
  cif_group_data_all,
  cause_code == "1"
)

group_levels <- levels(
  d$delta_group
)

cif_group_data$group <- factor(
  cif_group_data$group,
  levels = group_levels
)

write.csv(
  cif_group_data,
  file = "result_cumulative_incidence_by_delta_group.csv",
  row.names = FALSE
)

#-----------------------------------------------------------
# Gray検定
#-----------------------------------------------------------

gray_p <- NA_real_

if (
  !is.null(cif_group$Tests) &&
  "1" %in% rownames(cif_group$Tests) &&
  "pv" %in% colnames(cif_group$Tests)
) {
  gray_p <- cif_group$Tests["1", "pv"]
}

gray_subtitle <- if (is.na(gray_p)) {
  "Unadjusted cumulative incidence function"
} else {
  paste0(
    "Unadjusted cumulative incidence function; ",
    "Gray's test: ",
    format.pval(
      gray_p,
      digits = 3,
      eps = 0.001
    )
  )
}

gray_results <- data.frame(
  cause = 1,
  outcome = "Medication self-management",
  gray_p_value = gray_p
)

write.csv(
  gray_results,
  file = "result_gray_test_delta_group.csv",
  row.names = FALSE
)

#-----------------------------------------------------------
# 群別人数を凡例に表示
#-----------------------------------------------------------

group_n <- table(
  d$delta_group
)

legend_labels <- paste0(
  names(group_n),
  " (n = ",
  as.integer(group_n),
  ")"
)

names(legend_labels) <- names(
  group_n
)

maximum_followup_group <- ceiling(
  max(d$fu_days, na.rm = TRUE) / 30
) * 30

y_upper_group <- calculate_y_upper(
  cif_group_data$estimate
)

plot_colors_all <- c(
  "No simplification / increase" = "#0072B2",
  ">0 to 5 points" = "#E69F00",
  ">5 points" = "#009E73"
)

plot_linetypes_all <- c(
  "No simplification / increase" = "solid",
  ">0 to 5 points" = "longdash",
  ">5 points" = "dotdash"
)

plot_colors <- plot_colors_all[
  group_levels
]

plot_linetypes <- plot_linetypes_all[
  group_levels
]

#-----------------------------------------------------------
# 群別CIFプロット
#-----------------------------------------------------------

p_cif_group <- ggplot(
  cif_group_data,
  aes(
    x = time,
    y = estimate,
    colour = group,
    linetype = group,
    group = group
  )
) +
  geom_step(
    linewidth = 1.15,
    direction = "hv"
  ) +
  scale_colour_manual(
    name = "MRCI-J simplification",
    values = plot_colors,
    breaks = group_levels,
    labels = legend_labels[group_levels],
    drop = FALSE
  ) +
  scale_linetype_manual(
    name = "MRCI-J simplification",
    values = plot_linetypes,
    breaks = group_levels,
    labels = legend_labels[group_levels],
    drop = FALSE
  ) +
  scale_x_continuous(
    name = "Days after the day-14 landmark",
    breaks = seq(
      0,
      maximum_followup_group,
      by = 30
    ),
    expand = expansion(
      mult = c(0, 0.01)
    )
  ) +
  scale_y_continuous(
    name = paste0(
      "Cumulative incidence of ",
      "medication self-management"
    ),
    labels = scales::label_percent(
      accuracy = 1
    ),
    breaks = scales::pretty_breaks(
      n = 6
    ),
    expand = expansion(
      mult = c(0, 0.02)
    )
  ) +
  coord_cartesian(
    xlim = c(0, maximum_followup_group),
    ylim = c(0, y_upper_group),
    clip = "off"
  ) +
  labs(
    title = paste0(
      "Cumulative incidence of ",
      "medication self-management"
    ),
    subtitle = gray_subtitle,
    caption = paste0(
      "Competing events were handled as competing risks. ",
      "MRCI-J categories are used for descriptive ",
      "visualization only."
    )
  ) +
  theme_classic(
    base_size = 13
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 15
    ),
    plot.subtitle = element_text(
      size = 11,
      margin = margin(b = 10)
    ),
    axis.title = element_text(
      face = "bold"
    ),
    axis.text = element_text(
      colour = "black"
    ),
    legend.position = "bottom",
    legend.title = element_text(
      face = "bold"
    ),
    legend.text = element_text(
      size = 10
    ),
    legend.key.width = grid::unit(
      1.7,
      "cm"
    ),
    legend.box = "vertical",
    plot.caption = element_text(
      hjust = 0,
      size = 9,
      colour = "gray30",
      margin = margin(t = 10)
    ),
    plot.margin = margin(
      t = 10,
      r = 15,
      b = 10,
      l = 10
    )
  ) +
  guides(
    colour = guide_legend(
      nrow = 1,
      byrow = TRUE
    ),
    linetype = guide_legend(
      nrow = 1,
      byrow = TRUE
    )
  )

print(p_cif_group)

ggsave(
  filename = "plot_cumulative_incidence_by_delta_group.png",
  plot = p_cif_group,
  width = 10,
  height = 7,
  units = "in",
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = "plot_cumulative_incidence_by_delta_group.pdf",
  plot = p_cif_group,
  width = 10,
  height = 7,
  units = "in",
  bg = "white"
)

#-----------------------------------------------------------
# Fine–Grayモデル
#-----------------------------------------------------------

fit_fg <- FGR(
  Hist(fu_days, status) ~
    delta5 +
    age +
    sex +
    stroke_type +
    onset_to_admission +
    mrci0 +
    fim_motor14 +
    fim_cognitive14 +
    premorbid_manager +
    family_support +
    planned_destination,
  data = d,
  cause = 1
)

print(fit_fg)

fg_coef <- coef(
  fit_fg
)

fg_vcov <- vcov(
  fit_fg
)

fg_se <- sqrt(
  diag(fg_vcov)
)

fg_results <- data.frame(
  variable = names(fg_coef),
  sHR = exp(fg_coef),
  CI_lower = exp(
    fg_coef - 1.96 * fg_se
  ),
  CI_upper = exp(
    fg_coef + 1.96 * fg_se
  ),
  p_value = 2 * pnorm(
    -abs(fg_coef / fg_se)
  ),
  row.names = NULL
)

print(fg_results)

write.csv(
  fg_results,
  file = "result_fine_gray.csv",
  row.names = FALSE
)

#-----------------------------------------------------------
# 非線形性評価
# natural cubic spline
#-----------------------------------------------------------

fit_linear <- fit_cs_main

fit_spline <- coxph(
  Surv(fu_days, status == 1) ~
    ns(delta5, df = 3) +
    age +
    sex +
    stroke_type +
    onset_to_admission +
    mrci0 +
    fim_motor14 +
    fim_cognitive14 +
    premorbid_manager +
    family_support +
    planned_destination,
  data = d,
  ties = "efron",
  x = TRUE,
  y = TRUE
)

print(
  summary(fit_spline)
)

nonlinearity_test <- anova(
  fit_linear,
  fit_spline,
  test = "LRT"
)

print(nonlinearity_test)

capture.output(
  nonlinearity_test,
  file = "result_nonlinearity_lrt.txt"
)

#-----------------------------------------------------------
# スプライン曲線作成用データ
#-----------------------------------------------------------

delta_grid <- seq(
  from = unname(
    quantile(
      d$delta_mrci14,
      0.02,
      na.rm = TRUE
    )
  ),
  to = unname(
    quantile(
      d$delta_mrci14,
      0.98,
      na.rm = TRUE
    )
  ),
  length.out = 200
)

newdata_spline <- data.frame(
  delta5 = delta_grid / 5,
  age = median(d$age, na.rm = TRUE),
  sex = factor(
    "Male",
    levels = levels(d$sex)
  ),
  stroke_type = factor(
    "Ischemic",
    levels = levels(d$stroke_type)
  ),
  onset_to_admission = median(
    d$onset_to_admission,
    na.rm = TRUE
  ),
  mrci0 = median(
    d$mrci0,
    na.rm = TRUE
  ),
  fim_motor14 = median(
    d$fim_motor14,
    na.rm = TRUE
  ),
  fim_cognitive14 = median(
    d$fim_cognitive14,
    na.rm = TRUE
  ),
  premorbid_manager = factor(
    "Self",
    levels = levels(d$premorbid_manager)
  ),
  family_support = factor(
    "No",
    levels = levels(d$family_support)
  ),
  planned_destination = factor(
    "Home",
    levels = levels(d$planned_destination)
  )
)

pred_spline <- predict(
  fit_spline,
  newdata = newdata_spline,
  type = "lp",
  se.fit = TRUE
)

ref_data <- newdata_spline[
  1,
  ,
  drop = FALSE
]

ref_data$delta5 <- 0

ref_lp <- as.numeric(
  predict(
    fit_spline,
    newdata = ref_data,
    type = "lp"
  )
)

plot_data <- data.frame(
  delta_mrci14 = delta_grid,
  HR = exp(
    pred_spline$fit - ref_lp
  ),
  lower = exp(
    pred_spline$fit -
      ref_lp -
      1.96 * pred_spline$se.fit
  ),
  upper = exp(
    pred_spline$fit -
      ref_lp +
      1.96 * pred_spline$se.fit
  )
)

write.csv(
  plot_data,
  file = "result_spline_curve.csv",
  row.names = FALSE
)

p_spline <- ggplot(
  plot_data,
  aes(
    x = delta_mrci14,
    y = HR
  )
) +
  geom_ribbon(
    aes(
      ymin = lower,
      ymax = upper
    ),
    fill = "#56B4E9",
    alpha = 0.25
  ) +
  geom_line(
    colour = "#0072B2",
    linewidth = 1.2
  ) +
  geom_hline(
    yintercept = 1,
    linetype = "dashed",
    colour = "gray40"
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dotted",
    colour = "gray40"
  ) +
  scale_x_continuous(
    name = "MRCI-J simplification score"
  ) +
  scale_y_continuous(
    name = paste0(
      "Adjusted cause-specific ",
      "hazard ratio"
    )
  ) +
  labs(
    title = paste0(
      "Nonlinear association between ",
      "MRCI-J simplification and outcome"
    ),
    subtitle = paste0(
      "Natural cubic spline; ",
      "reference = 0-point simplification"
    )
  ) +
  theme_classic(
    base_size = 13
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 15
    ),
    plot.subtitle = element_text(
      size = 11,
      margin = margin(b = 10)
    ),
    axis.title = element_text(
      face = "bold"
    ),
    axis.text = element_text(
      colour = "black"
    )
  )

print(p_spline)

ggsave(
  filename = "plot_spline_delta_mrci.png",
  plot = p_spline,
  width = 9.5,
  height = 7.5,
  units = "in",
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = "plot_spline_delta_mrci.pdf",
  plot = p_spline,
  width = 9.5,
  height = 7.5,
  units = "in",
  bg = "white"
)

#-----------------------------------------------------------
# 解析オブジェクトを保存
#-----------------------------------------------------------

saveRDS(
  list(
    fit_cs_main = fit_cs_main,
    fit_cs_judgement = fit_cs_judgement,
    fit_mrci14 = fit_mrci14,
    fit_fg = fit_fg,
    fit_spline = fit_spline,
    ph_test = ph_test,
    cif_overall = cif_overall,
    cif_group = cif_group
  ),
  file = "main_analysis_objects.rds"
)

#-----------------------------------------------------------
# Session information
#-----------------------------------------------------------

capture.output(
  sessionInfo(),
  file = "sessionInfo_main_analysis.txt"
)

cat("\n主要解析が完了しました。\n")
