############################################################
# 14_mrci_discharge_missing_data.R
#
# 多重代入（mice）による欠測処理
#
# 主要曝露・主要アウトカムは代入せず、共変量のみを代入する。
# 各代入データでmodified Poisson回帰＋ロバスト分散を実行し、
# Rubin則で統合する。
############################################################

required_packages <- c(
  "rprojroot", "mice", "sandwich"
)

not_installed <- setdiff(
  required_packages,
  rownames(installed.packages())
)

if (length(not_installed) > 0) {
  install.packages(not_installed, dependencies = TRUE)
}

library(mice)
library(sandwich)

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

data_file <- file.path(DATA_DIR, "mrci_discharge.rds")

if (!file.exists(data_file)) {
  data_file <- file.path(DATA_DIR, "sample_mrci_discharge.rds")
}

d <- readRDS(data_file)

d$mrci5 <- d$mrci0 / 5

# premorbid_mrs を ordered から unordered factor に変換し、
# 主要解析（13番）のダミー変数調整と一致させる。
d$premorbid_mrs <- factor(d$premorbid_mrs, ordered = FALSE)

mi_variables <- c(
  "discharge_complete_self",
  "mrci5",
  "age",
  "sex",
  "premorbid_manager",
  "premorbid_mrs",
  "stroke_type",
  "fim_motor0",
  "fim_cognitive0",
  "family_support0",
  "ward",
  "admission_year",
  "comorbidity_index",
  "nihss0",
  "aphasia",
  "neglect"
)

mi_data <- d[, mi_variables]

#-----------------------------------------------------------
# 代入法
#-----------------------------------------------------------

ini <- mice(
  mi_data,
  maxit = 0,
  printFlag = FALSE
)

method <- ini$method
predictor_matrix <- ini$predictorMatrix

# 主要曝露・主要アウトカムは代入しない
method["discharge_complete_self"] <- ""
method["mrci5"] <- ""

predictor_matrix[
  c("discharge_complete_self", "mrci5"),
  ] <- 0

# ただし、他の変数の代入予測には使用する
predictor_matrix[
  ,
  c("discharge_complete_self", "mrci5")
] <- 1

diag(predictor_matrix) <- 0

imp <- mice(
  mi_data,
  m = 30,
  maxit = 20,
  method = method,
  predictorMatrix = predictor_matrix,
  seed = 20260724,
  printFlag = TRUE
)

formula_model3 <-
  discharge_complete_self ~
  mrci5 +
  age +
  sex +
  premorbid_manager +
  premorbid_mrs +
  stroke_type +
  fim_motor0 +
  fim_cognitive0 +
  family_support0 +
  ward +
  admission_year

#-----------------------------------------------------------
# 各代入データで修正Poisson＋ロバスト分散
#-----------------------------------------------------------

completed_data <- lapply(
  seq_len(imp$m),
  function(i) complete(imp, i)
)

fits <- lapply(
  completed_data,
  function(x) {
    glm(
      formula_model3,
      data = x,
      family = poisson(link = "log"),
      x = TRUE,
      y = TRUE
    )
  }
)

coefficients_matrix <- do.call(
  rbind,
  lapply(fits, coef)
)

robust_covariances <- lapply(
  fits,
  function(fit) {
    sandwich::vcovHC(fit, type = "HC0")
  }
)

terms <- colnames(coefficients_matrix)

# Rubin則による係数ごとの統合
pooled_results <- do.call(
  rbind,
  lapply(
    seq_along(terms),
    function(j) {

      q <- coefficients_matrix[, j]

      u <- vapply(
        robust_covariances,
        function(V) V[j, j],
        numeric(1)
      )

      pooled <- mice::pool.scalar(
        Q = q,
        U = u,
        n = nrow(mi_data),
        k = length(terms)
      )

      estimate <- pooled$qbar
      standard_error <- sqrt(pooled$t)

      data.frame(
        variable = terms[j],
        coefficient = estimate,
        robust_se = standard_error,
        df = pooled$df,
        relative_risk = exp(estimate),
        ci_lower = exp(
          estimate -
            qt(0.975, pooled$df) * standard_error
        ),
        ci_upper = exp(
          estimate +
            qt(0.975, pooled$df) * standard_error
        ),
        p_value = 2 * pt(
          abs(estimate / standard_error),
          df = pooled$df,
          lower.tail = FALSE
        )
      )
    }
  )
)

write.csv(
  pooled_results,
  file.path(
    TABLE_DIR,
    "multiple_imputation_modified_poisson.csv"
  ),
  row.names = FALSE
)

saveRDS(
  imp,
  file.path(
    ROOT,
    "results",
    "multiple_imputation_object.rds"
  )
)

print(
  subset(
    pooled_results,
    variable == "mrci5"
  )
)
