############################################################
# 01_make_sample_data.R
# 脳卒中患者・MRCI-J・自己管理獲得研究
# 完全に架空のサンプルデータを作成するコード
############################################################

set.seed(20260718)

# 主要ランドマーク解析対象者数
n <- 800

#-----------------------------------------------------------
# 補助関数
#-----------------------------------------------------------

# 値を範囲内に制限する関数
clamp <- function(x, lower, upper) {
  pmin(pmax(x, lower), upper)
}

# 0.5点刻みに丸める関数
round_half <- function(x) {
  round(x * 2) / 2
}

#-----------------------------------------------------------
# 基本属性
#-----------------------------------------------------------

id <- sprintf("P%04d", seq_len(n))

age <- round(clamp(rnorm(n, mean = 68, sd = 12), 20, 95))

sex <- factor(
  rbinom(n, 1, 0.43),
  levels = c(0, 1),
  labels = c("Male", "Female")
)

stroke_type <- factor(
  sample(
    c("Ischemic", "ICH", "SAH"),
    size = n,
    replace = TRUE,
    prob = c(0.64, 0.29, 0.07)
  ),
  levels = c("Ischemic", "ICH", "SAH")
)

onset_to_admission <- round(
  clamp(rnorm(n, mean = 38, sd = 15), 7, 90)
)

#-----------------------------------------------------------
# 発症前の服薬管理者
#-----------------------------------------------------------

premorbid_manager <- factor(
  sample(
    c("Self", "Family", "Care_staff", "No_medication"),
    size = n,
    replace = TRUE,
    prob = c(0.66, 0.20, 0.10, 0.04)
  ),
  levels = c("Self", "Family", "Care_staff", "No_medication")
)

#-----------------------------------------------------------
# 入棟時MRCI-J
# 0.5点刻み、最低2点
#-----------------------------------------------------------

mrci0 <- 5 + rgamma(n, shape = 4.5, scale = 3.3)
mrci0 <- round_half(clamp(mrci0, 2, 60))

#-----------------------------------------------------------
# 入棟時から14日目までのMRCI-J変化
# 入棟時MRCI-Jが高い患者ほど低下余地が大きい設定
#-----------------------------------------------------------

latent_simplification14 <-
  0.10 * (mrci0 - 15) +
  rnorm(n, mean = 1.2, sd = 3.6)

delta_mrci14 <- round_half(
  clamp(latent_simplification14, -12, 18)
)

mrci14 <- round_half(
  pmax(2, mrci0 - delta_mrci14)
)

# 最低値制約を反映して単純化量を再計算
delta_mrci14 <- round_half(mrci0 - mrci14)

#-----------------------------------------------------------
# 14日目FIM
#-----------------------------------------------------------

fim_motor14 <- round(
  clamp(
    58 -
      0.45 * (age - 68) +
      ifelse(stroke_type == "SAH", -5, 0) +
      ifelse(stroke_type == "ICH", -3, 0) +
      rnorm(n, 0, 15),
    13, 91
  )
)

fim_cognitive14 <- round(
  clamp(
    25 -
      0.18 * (age - 68) +
      ifelse(stroke_type == "SAH", -2, 0) +
      rnorm(n, 0, 6),
    5, 35
  )
)

#-----------------------------------------------------------
# 家族支援可能性
#-----------------------------------------------------------

family_lp <-
  -0.2 +
  0.7 * (premorbid_manager == "Family") +
  0.3 * (age >= 75)

family_support <- factor(
  rbinom(n, 1, plogis(family_lp)),
  levels = c(0, 1),
  labels = c("No", "Yes")
)

#-----------------------------------------------------------
# 14日目時点の予定退院先
#-----------------------------------------------------------

home_lp <-
  1.0 +
  0.025 * (fim_motor14 - 50) +
  0.055 * (fim_cognitive14 - 20) -
  0.02 * (age - 68) +
  0.4 * (family_support == "Yes")

p_home <- plogis(home_lp)

planned_destination <- ifelse(
  runif(n) < p_home,
  "Home",
  ifelse(runif(n) < 0.65, "Facility", "Undecided")
)

planned_destination <- factor(
  planned_destination,
  levels = c("Home", "Facility", "Undecided")
)

#-----------------------------------------------------------
# 14日目時点の医療者による自己管理判断
# MRCI-J、FIM、発症前管理者等から生成
#-----------------------------------------------------------

judgement_score <-
  0.045 * (fim_motor14 - 50) +
  0.14 * (fim_cognitive14 - 20) -
  0.035 * (mrci14 - 15) +
  0.7 * (premorbid_manager == "Self") +
  0.3 * (planned_destination == "Home") +
  rnorm(n, 0, 0.8)

clinical_judgement14 <- cut(
  judgement_score,
  breaks = c(-Inf, -0.4, 0.7, Inf),
  labels = c(
    "Currently_difficult",
    "Reassess_or_conditional",
    "Suitable"
  ),
  ordered_result = TRUE
)

#-----------------------------------------------------------
# ランドマーク後のイベント生成
#
# status:
# 0 = 行政的打切り
# 1 = 自己管理獲得・再獲得
# 2 = 競合イベント
#-----------------------------------------------------------

# 主要曝露を5点単位に変換
delta5 <- delta_mrci14 / 5

# 自己管理獲得ハザード
lp_event <-
  log(1.25) * delta5 -
  0.018 * (age - 68) +
  0.018 * (fim_motor14 - 50) +
  0.065 * (fim_cognitive14 - 20) +
  0.45 * (premorbid_manager == "Self") +
  0.20 * (family_support == "Yes") +
  0.25 * (planned_destination == "Home") -
  0.10 * (stroke_type == "ICH") -
  0.20 * (stroke_type == "SAH")

# 競合イベントハザード
lp_competing <-
  0.018 * (age - 68) -
  0.015 * (fim_motor14 - 50) -
  0.030 * (fim_cognitive14 - 20) +
  0.45 * (planned_destination == "Facility") +
  0.20 * (planned_destination == "Undecided")

# 日単位ハザード
rate_event <- 0.010 * exp(lp_event)
rate_competing <- 0.008 * exp(lp_competing)

time_event <- rexp(n, rate = rate_event)
time_competing <- rexp(n, rate = rate_competing)

# 最大追跡期間：ランドマーク後120日
administrative_censor <- rep(120, n)

fu_days <- pmin(
  time_event,
  time_competing,
  administrative_censor
)

status <- ifelse(
  time_event <= time_competing &
    time_event <= administrative_censor,
  1,
  ifelse(
    time_competing < time_event &
      time_competing <= administrative_censor,
    2,
    0
  )
)

# 0日を避ける
fu_days <- round(pmax(fu_days, 0.1), 1)

#-----------------------------------------------------------
# 競合イベントの種類
#-----------------------------------------------------------

competing_type <- rep(NA_character_, n)

competing_types <- c(
  "Discharge_without_self_management",
  "Transfer_to_acute_hospital",
  "Transfer_to_other_hospital",
  "Death",
  "All_target_medications_stopped"
)

competing_type[status == 2] <- sample(
  competing_types,
  size = sum(status == 2),
  replace = TRUE,
  prob = c(0.72, 0.14, 0.06, 0.02, 0.06)
)

competing_type <- factor(
  competing_type,
  levels = competing_types
)

#-----------------------------------------------------------
# 30日ランドマーク感度解析用変数
#
# 主要追跡開始は15日目。
# 30日目終了までイベントなし：
# 15日目から16日超経過している患者を適格とする。
#-----------------------------------------------------------

eligible30 <- fu_days > 16

# 14～30日目に追加の処方変化が生じる設定
additional_simplification30 <- round_half(
  clamp(
    0.05 * (mrci14 - 15) + rnorm(n, 0.8, 2.5),
    -8, 12
  )
)

mrci30 <- rep(NA_real_, n)
mrci30[eligible30] <- round_half(
  pmax(
    2,
    mrci14[eligible30] -
      additional_simplification30[eligible30]
  )
)

delta_mrci30 <- rep(NA_real_, n)
delta_mrci30[eligible30] <-
  round_half(mrci0[eligible30] - mrci30[eligible30])

fu_days30 <- rep(NA_real_, n)
fu_days30[eligible30] <-
  round(fu_days[eligible30] - 16, 1)

status30 <- rep(NA_integer_, n)
status30[eligible30] <- status[eligible30]

#-----------------------------------------------------------
# データフレーム作成
#-----------------------------------------------------------

sample_data <- data.frame(
  id = id,
  age = age,
  sex = sex,
  stroke_type = stroke_type,
  onset_to_admission = onset_to_admission,
  mrci0 = mrci0,
  mrci14 = mrci14,
  delta_mrci14 = delta_mrci14,
  fim_motor14 = fim_motor14,
  fim_cognitive14 = fim_cognitive14,
  premorbid_manager = premorbid_manager,
  family_support = family_support,
  planned_destination = planned_destination,
  clinical_judgement14 = clinical_judgement14,
  fu_days = fu_days,
  status = status,
  competing_type = competing_type,
  eligible30 = eligible30,
  mrci30 = mrci30,
  delta_mrci30 = delta_mrci30,
  fu_days30 = fu_days30,
  status30 = status30
)

# statusのラベル変数
sample_data$status_label <- factor(
  sample_data$status,
  levels = c(0, 1, 2),
  labels = c(
    "Censored",
    "Self_management",
    "Competing_event"
  )
)

# データ確認
print(head(sample_data, 10))
print(table(sample_data$status_label))
print(table(sample_data$eligible30))

# CSV保存
write.csv(
  sample_data,
  file = "sample_stroke_mrci_landmark.csv",
  row.names = FALSE,
  na = ""
)

# R形式でも保存
saveRDS(
  sample_data,
  file = "sample_stroke_mrci_landmark.rds"
)

cat("\nサンプルデータを保存しました。\n")
cat("CSV: sample_stroke_mrci_landmark.csv\n")
cat("RDS: sample_stroke_mrci_landmark.rds\n")
