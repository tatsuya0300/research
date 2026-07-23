############################################################
# 08_run_protocol_analysis.R
# 全解析一括実行
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

#============================================================
# 実行するスクリプト
#============================================================

scripts <- file.path(
  R_DIRECTORY,
  c(
    "05_generate_protocol_sample_data.R",
    "06_primary_sequential_landmark_analysis.R",
    "07_medication_error_analysis.R"
  )
)

missing_scripts <- scripts[
  !file.exists(scripts)
]

if (length(missing_scripts) > 0) {

  stop(
    paste0(
      "以下の解析スクリプトがありません:\n",
      paste(
        missing_scripts,
        collapse = "\n"
      )
    )
  )
}

#============================================================
# 全解析実行
#============================================================

for (script in scripts) {

  cat(
    "\n========================================\n"
  )

  cat(
    "Running:",
    script,
    "\n"
  )

  cat(
    "========================================\n"
  )

  source(
    script,
    echo = FALSE,
    chdir = FALSE,
    encoding = "UTF-8"
  )
}

capture.output(
  sessionInfo(),
  file = file.path(
    RESULTS_DIRECTORY,
    "sessionInfo_all.txt"
  )
)

cat(
  "\n全解析が完了しました。\n"
)

cat(
  "データ保存先:",
  DATA_DIRECTORY,
  "\n"
)

cat(
  "結果保存先:",
  RESULTS_DIRECTORY,
  "\n"
)
