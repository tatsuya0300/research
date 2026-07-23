############################################################
# 08_run_protocol_analysis.R
# 全解析一括実行
############################################################

options(
  stringsAsFactors = FALSE,
  warn = 1
)

scripts <- c(
  "R/05_generate_protocol_sample_data.R",
  "R/06_primary_sequential_landmark_analysis.R",
  "R/07_medication_error_analysis.R"
)

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
    chdir = FALSE
  )
}

capture.output(
  sessionInfo(),
  file = "results/sessionInfo_all.txt"
)

cat("\n全解析が完了しました。\n")
