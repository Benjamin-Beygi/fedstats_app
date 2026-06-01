# Tutorial 3: Linear regression
#
# You want to know which patient characteristics predict an outcome.
# For example: does age, sex, or preoperative pain predict
# how much pain the patient reports at 12 months?
#
# Linear regression gives you a number (coefficient) for each predictor.
# Positive coefficient = higher predictor value linked to higher outcome.
# Negative coefficient = higher predictor value linked to lower outcome.

library(fedstats)

ANALYSIS_TITLE <- "Simple Linear Regression"

VARS_SPEC <- list(
  nrs_arm_12m   = list(type = "numeric", min = 0, max = 10),
  age           = list(type = "numeric", min = 18, max = 100),
  sexM          = list(type = "binary"),
  nrs_arm_preop = list(type = "numeric", min = 0, max = 10)
)

# Leave this block as it is
if (!exists("servers", inherits = FALSE)) {
  .csvs <- sort(list.files("data", pattern = "[.]csv$", full.names = TRUE))
  if (!length(.csvs)) stop("No CSV files found in the data folder")
  servers <- lapply(.csvs, create_server)
}

clear_outputs()

# Run the regression
# Left of ~ is the outcome  (what you want to predict)
# Right of ~ are predictors (what you think influences the outcome)
# Use + to add more predictors
fit <- fed_lm(servers, nrs_arm_12m ~ age + sexM + nrs_arm_preop)

# How many patients were used
cat("Patients included:", fit$N, "\n")

# Print each predictor with its coefficient and p-value
cat("\nResults\n")
for (i in seq_along(fit$coefficients)) {
  name <- names(fit$coefficients)[i]
  coef <- round(fit$coefficients[i], 3)
  pval <- round(fit$p[i], 4)
  sig  <- if (pval < 0.05) " *" else ""
  cat(sprintf("  %-20s  coefficient %+.3f  p = %.4f%s\n", name, coef, pval, sig))
}

# Build a results table for the app
results_table <- data.frame(
  Predictor   = names(fit$coefficients),
  Coefficient = round(fit$coefficients, 3),
  Std_Error   = round(fit$se, 3),
  p_value     = round(fit$p, 4)
)

register_output(
  "Regression results",
  results_table,
  "table",
  paste("Linear regression predicting nrs_arm_12m  N =", fit$N)
)

cat("Done\n")
