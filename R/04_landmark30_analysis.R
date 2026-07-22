############################################################
# 04_landmark30_analysis.R
# 30日ランドマーク感度解析：修正版
############################################################

#-----------------------------------------------------------
# 必要パッケージ
#-----------------------------------------------------------

required_packages <- c(
  "survival",
  "cmprsk",
  "riskRegression",
  "prodlim",
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
library(ggplot2)

#-----------------------------------------------------------
# 補助関数
#-----------------------------------------------------------

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

        cause_code <- sub(
          pattern = "^.* ",
          replacement = "",
          x = curve_name
        )

        data.frame(
          curve_name = curve_name,
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
# 変数型設定
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

d$eligible30 <- as.logical(
  d$eligible30
)

#-----------------------------------------------------------
# 30日ランドマーク適格患者
#-----------------------------------------------------------

d30 <- subset(
  d,
  eligible30 &
    !is.na(mrci30) &
    !is.na(fu_days30) &
    !is.na(status30)
)

if (nrow(d30) == 0) {
  stop(
    "30日ランドマーク解析の適格患者が存在しません。"
  )
}

d30$delta30_5 <- d30$delta_mrci30 / 5

stopifnot(
  all(d30$fu_days30 > 0, na.rm = TRUE)
)

stopifnot(
  all(d30$status30 %in% c(0, 1, 2))
)

cat(
  "\n30日ランドマーク対象者数:",
  nrow(d30),
  "\n"
)

cat(
  "自己管理獲得数:",
  sum(d30$status30 == 1, na.rm = TRUE),
  "\n"
)

cat(
  "競合イベント数:",
  sum(d30$status30 == 2, na.rm = TRUE),
  "\n"
)

cat(
  "行政的打切り数:",
  sum(d30$status30 == 0, na.rm = TRUE),
  "\n"
)

#-----------------------------------------------------------
# 30日ランドマークCoxモデル
#
# サンプルデータでは14日目のFIM・予定退院先等を使用する。
# 実データでは30日目時点の共変量を用いることが望ましい。
#-----------------------------------------------------------

fit_cs_30 <- coxph(
  Surv(fu_days30, status30 == 1) ~
    delta30_5 +
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
  data = d30,
  ties = "efron",
  x = TRUE,
  y = TRUE
)

print(
  summary(fit_cs_30)
)

#-----------------------------------------------------------
# Cox回帰結果表
#-----------------------------------------------------------

results30 <- extract_cox_results(
  fit_cs_30
)

print(results30)

write.csv(
  results30,
  file = "result_landmark30_cox.csv",
  row.names = FALSE
)

primary_result30 <- subset(
  results30,
  variable == "delta30_5"
)

cat(
  "\n30日ランドマーク：MRCI-J 5点単純化当たりの結果\n"
)

print(primary_result30)

#-----------------------------------------------------------
# 比例ハザード仮定
#-----------------------------------------------------------

ph_test30 <- cox.zph(
  fit_cs_30,
  transform = "km"
)

print(ph_test30)

ph_results30 <- data.frame(
  variable = rownames(ph_test30$table),
  ph_test30$table,
  row.names = NULL,
  check.names = FALSE
)

write.csv(
  ph_results30,
  file = "result_landmark30_ph_test.csv",
  row.names = FALSE
)

png(
  filename = "plot_schoenfeld_delta30_5.png",
  width = 1000,
  height = 750,
  res = 120
)

par(
  mar = c(5.5, 5.5, 5, 2) + 0.1
)

plot(
  ph_test30["delta30_5"],
  main = "",
  xlab = "Time",
  ylab = "Scaled Schoenfeld residual"
)

title(
  main = paste0(
    "Schoenfeld residuals: ",
    "30-day MRCI-J simplification"
  ),
  line = 2,
  font.main = 2,
  cex.main = 1.2
)

abline(
  h = 0,
  lty = 2,
  col = "gray40"
)

dev.off()

#-----------------------------------------------------------
# 30日ランドマーク累積発生関数
#-----------------------------------------------------------

cif30 <- cuminc(
  ftime = d30$fu_days30,
  fstatus = d30$status30,
  cencode = 0
)

print(cif30)

#-----------------------------------------------------------
# cumincオブジェクトをデータフレーム化
#-----------------------------------------------------------

cif30_data <- cuminc_to_data(
  cif30
)

cif30_data$cause <- factor(
  cif30_data$cause_code,
  levels = c("1", "2"),
  labels = c(
    "Medication self-management",
    "Competing event"
  )
)

write.csv(
  cif30_data,
  file = "result_cumulative_incidence_landmark30.csv",
  row.names = FALSE
)

#-----------------------------------------------------------
# 軸の範囲
#-----------------------------------------------------------

maximum_followup30 <- ceiling(
  max(
    d30$fu_days30,
    na.rm = TRUE
  ) / 30
) * 30

y_upper30 <- calculate_y_upper(
  cif30_data$estimate
)

#-----------------------------------------------------------
# 30日ランドマークCIFプロット
#-----------------------------------------------------------

p_cif30 <- ggplot(
  cif30_data,
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
    name = "Days after the day-30 landmark",
    breaks = seq(
      0,
      maximum_followup30,
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
    xlim = c(0, maximum_followup30),
    ylim = c(0, y_upper30),
    clip = "off"
  ) +
  labs(
    title = "Cumulative incidence after the day-30 landmark",
    subtitle = paste0(
      "Medication self-management and competing events"
    ),
    caption = paste0(
      "The analysis included patients who remained ",
      "event-free and under observation at the ",
      "day-30 landmark."
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

print(p_cif30)

ggsave(
  filename = "plot_cumulative_incidence_landmark30.png",
  plot = p_cif30,
  width = 9,
  height = 7,
  units = "in",
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = "plot_cumulative_incidence_landmark30.pdf",
  plot = p_cif30,
  width = 9,
  height = 7,
  units = "in",
  bg = "white"
)

#-----------------------------------------------------------
# 解析オブジェクトを保存
#-----------------------------------------------------------

saveRDS(
  list(
    d30 = d30,
    fit_cs_30 = fit_cs_30,
    ph_test30 = ph_test30,
    cif30 = cif30
  ),
  file = "landmark30_analysis_objects.rds"
)

#-----------------------------------------------------------
# Session information
#-----------------------------------------------------------

capture.output(
  sessionInfo(),
  file = "sessionInfo_landmark30_analysis.txt"
)

cat(
  "\n30日ランドマーク解析が完了しました。\n"
)
