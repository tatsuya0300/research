############################################################
# 03_adjusted_risk.R
# 調整累積発生確率と絶対リスク差
############################################################

library(survival)
library(riskRegression)
library(prodlim)

#-----------------------------------------------------------
# データ読み込み・変数型設定
#-----------------------------------------------------------

d <- read.csv(
  "sample_stroke_mrci_landmark.csv",
  stringsAsFactors = FALSE
)

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
  levels = c("Home", "Facility", "Undecided")
)

d$delta5 <- d$delta_mrci14 / 5

#-----------------------------------------------------------
# Cause-specific Coxモデル
# CSCは全causeのモデルを内部で推定
#-----------------------------------------------------------

fit_csc <- CSC(
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
  data = d
)

print(fit_csc)

#-----------------------------------------------------------
# 比較するMRCI-J単純化量
# 例：0点と5点
#-----------------------------------------------------------

newdata_delta0 <- d
newdata_delta5 <- d

newdata_delta0$delta5 <- 0
newdata_delta5$delta5 <- 1  # 5点単純化

times <- c(30, 60, 90)

# 個人ごとの予測累積発生確率
risk_delta0 <- predictRisk(
  fit_csc,
  newdata = newdata_delta0,
  times = times,
  cause = 1
)

risk_delta5 <- predictRisk(
  fit_csc,
  newdata = newdata_delta5,
  times = times,
  cause = 1
)

# 全患者で標準化
standardized_risk0 <- colMeans(
  risk_delta0,
  na.rm = TRUE
)

standardized_risk5 <- colMeans(
  risk_delta5,
  na.rm = TRUE
)

risk_difference <- standardized_risk5 -
  standardized_risk0

risk_ratio <- standardized_risk5 /
  standardized_risk0

absolute_results <- data.frame(
  time_days = times,
  risk_delta0 = standardized_risk0,
  risk_delta5 = standardized_risk5,
  risk_difference = risk_difference,
  risk_ratio = risk_ratio
)

print(absolute_results)

write.csv(
  absolute_results,
  "result_adjusted_absolute_risk.csv",
  row.names = FALSE
)
