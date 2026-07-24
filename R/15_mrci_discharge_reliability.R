############################################################
# 15_mrci_discharge_reliability.R
#
# 評価者間信頼性
#
# 実データは以下のwide形式を想定:
#   id
#   mrci_total_r1, mrci_total_r2
#   mrci_a_r1, mrci_a_r2
#   mrci_b_r1, mrci_b_r2
#   mrci_c_r1, mrci_c_r2
#   management_level_r1, management_level_r2
#   complete_self_r1, complete_self_r2
#   trial_started_r1, trial_started_r2
#
# サンプル信頼性データがない場合は合成データから生成する。
############################################################

required_packages <- c(
  "rprojroot", "irr"
)

not_installed <- setdiff(
  required_packages,
  rownames(installed.packages())
)

if (length(not_installed) > 0) {
  install.packages(not_installed, dependencies = TRUE)
}

library(irr)

ROOT <- tryCatch(
  rprojroot::find_root(
    rprojroot::is_git_root |
      rprojroot::has_file("research.Rproj")
  ),
  error = function(e) getwd()
)

DATA_DIR <- file.path(ROOT, "data")
TABLE_DIR <- file.path(ROOT, "results", "tables")
dir.create(TABLE_DIR, recursive = TRUE, showWarnings = FALSE)

reliability_file <- file.path(
  DATA_DIR,
  "mrci_reliability.csv"
)

if (!file.exists(reliability_file)) {
  message(
    "mrci_reliability.csvがないため、",
    "サンプル信頼性データを作成します。"
  )

  d <- readRDS(
    file.path(DATA_DIR, "sample_mrci_discharge.rds")
  )

  set.seed(20260724)

  n_reliability <- max(
    30,
    ceiling(0.20 * nrow(d))
  )

  r <- d[
    sample(seq_len(nrow(d)), n_reliability),
    ,
    drop = FALSE
  ]

  reliability_data <- data.frame(
    id = r$id,

    mrci_total_r1 = r$mrci0,
    mrci_total_r2 = pmax(
      0,
      round((r$mrci0 + rnorm(n_reliability, 0, 0.7)) * 2) / 2
    ),

    mrci_a_r1 = r$mrci_a0,
    mrci_a_r2 = pmax(
      0,
      round((r$mrci_a0 + rnorm(n_reliability, 0, 0.3)) * 2) / 2
    ),

    mrci_b_r1 = r$mrci_b0,
    mrci_b_r2 = pmax(
      0,
      round((r$mrci_b0 + rnorm(n_reliability, 0, 0.5)) * 2) / 2
    ),

    mrci_c_r1 = r$mrci_c0,
    mrci_c_r2 = pmax(
      0,
      round((r$mrci_c0 + rnorm(n_reliability, 0, 0.4)) * 2) / 2
    ),

    management_level_r1 =
      r$discharge_management_level,

    management_level_r2 =
      pmin(
        4,
        pmax(
          1,
          r$discharge_management_level +
            sample(
              c(-1, 0, 1),
              n_reliability,
              replace = TRUE,
              prob = c(0.05, 0.90, 0.05)
            )
        )
      ),

    complete_self_r1 =
      r$discharge_complete_self,

    complete_self_r2 =
      ifelse(
        runif(n_reliability) < 0.95,
        r$discharge_complete_self,
        1 - r$discharge_complete_self
      ),

    trial_started_r1 =
      r$trial_started,

    trial_started_r2 =
      ifelse(
        runif(n_reliability) < 0.94,
        r$trial_started,
        1 - r$trial_started
      )
  )

  write.csv(
    reliability_data,
    reliability_file,
    row.names = FALSE
  )
}

r <- read.csv(reliability_file)

#-----------------------------------------------------------
# ICC：絶対一致、単一測定
#-----------------------------------------------------------

icc_variables <- c(
  "mrci_total",
  "mrci_a",
  "mrci_b",
  "mrci_c"
)

icc_results <- do.call(
  rbind,
  lapply(
    icc_variables,
    function(variable) {

      ratings <- r[
        ,
        c(
          paste0(variable, "_r1"),
          paste0(variable, "_r2")
        )
      ]

      result <- irr::icc(
        ratings,
        model = "twoway",
        type = "agreement",
        unit = "single"
      )

      data.frame(
        variable = variable,
        ICC = result$value,
        ci_lower = result$lbound,
        ci_upper = result$ubound,
        n = result$subjects
      )
    }
  )
)

#-----------------------------------------------------------
# κ係数
#-----------------------------------------------------------

complete_kappa <- irr::kappa2(
  r[, c("complete_self_r1", "complete_self_r2")],
  weight = "unweighted"
)

trial_kappa <- irr::kappa2(
  r[, c("trial_started_r1", "trial_started_r2")],
  weight = "unweighted"
)

management_kappa <- irr::kappa2(
  r[, c("management_level_r1", "management_level_r2")],
  weight = "squared"
)

kappa_results <- data.frame(
  variable = c(
    "complete_self",
    "trial_started",
    "management_level"
  ),
  kappa = c(
    complete_kappa$value,
    trial_kappa$value,
    management_kappa$value
  ),
  method = c(
    "Cohen unweighted kappa",
    "Cohen unweighted kappa",
    "Weighted kappa, squared weights"
  )
)

write.csv(
  icc_results,
  file.path(TABLE_DIR, "interrater_icc.csv"),
  row.names = FALSE
)

write.csv(
  kappa_results,
  file.path(TABLE_DIR, "interrater_kappa.csv"),
  row.names = FALSE
)

print(icc_results)
print(kappa_results)
