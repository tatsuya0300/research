############################################################
# 16_run_mrci_discharge_analysis.R
# 入院時MRCI-Jと退院時完全内服自己管理の研究用
# 全解析一括実行
############################################################

if (requireNamespace("rprojroot", quietly = TRUE)) {
  ROOT <- rprojroot::find_root(
    rprojroot::is_git_root |
      rprojroot::has_file("research.Rproj")
  )
} else {
  ROOT <- getwd()
}

scripts <- file.path(
  ROOT,
  "R",
  c(
    "12_generate_mrci_discharge_sample.R",
    "13_mrci_discharge_primary_analysis.R",
    "14_mrci_discharge_missing_data.R",
    "15_mrci_discharge_reliability.R"
  )
)

missing_scripts <- scripts[!file.exists(scripts)]

if (length(missing_scripts) > 0) {
  stop(
    "不足スクリプト:\n",
    paste(missing_scripts, collapse = "\n")
  )
}

for (script in scripts) {

  cat(
    "\n========================================\n",
    "Running: ", basename(script), "\n",
    "========================================\n",
    sep = ""
  )

  source(
    script,
    echo = FALSE,
    chdir = FALSE,
    encoding = "UTF-8"
  )
}

cat("\n全解析完了\n")
