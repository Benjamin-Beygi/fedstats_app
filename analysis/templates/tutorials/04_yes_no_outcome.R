# Tutorial 4: Predicting a yes/no outcome (logistic regression)
#
# When your outcome is binary (yes/no, satisfied/not satisfied,
# complication/no complication) you use logistic regression.
# Instead of a coefficient it gives you an Odds Ratio (OR).
#
# OR > 1 means the predictor increases the odds of the outcome.
# OR < 1 means the predictor decreases the odds.
# OR = 1 means no effect.

library(fedstats)

ANALYSIS_TITLE <- "Simple Logistic Regression"

VARS_SPEC <- list(
  patient_satisfied_12m = list(type = "binary"),
  age                   = list(type = "numeric", min = 18, max = 100),
  sexM                  = list(type = "binary"),
  nrs_arm_preop         = list(type = "numeric", min = 0, max = 10)
)

# Leave this block as it is
if (!exists("servers", inherits = FALSE)) {
  .csvs <- sort(list.files("data", pattern = "[.]csv$", full.names = TRUE))
  if (!length(.csvs)) stop("No CSV files found in the data folder")
  servers <- lapply(.csvs, create_server)
}

clear_outputs()

# Run the logistic regression
# Same formula style as linear regression
# robust_cluster = TRUE is recommended when you have multiple sites
fit <- fed_logistic_newton(
  servers,
  patient_satisfied_12m ~ age + sexM + nrs_arm_preop,
  robust_cluster = TRUE
)

# Choose which standard errors to use
se <- if (!is.null(fit$se_robust)) fit$se_robust else fit$se

# Compute p-values
z      <- fit$coefficients / se
pvals  <- 2 * (1 - pnorm(abs(z)))

# Compute odds ratios and 95% confidence intervals
OR     <- exp(fit$coefficients)
CI_low <- exp(fit$coefficients - 1.96 * se)
CI_hi  <- exp(fit$coefficients + 1.96 * se)

# Print results
cat("Patients included:", fit$N, "\n\n")
cat("Results\n")
for (i in seq_along(fit$coefficients)) {
  name <- names(fit$coefficients)[i]
  if (name == "(Intercept)") next
  sig <- if (pvals[i] < 0.05) " *" else ""
  cat(sprintf("  %-25s  OR = %.2f  [%.2f to %.2f]  p = %.4f%s\n",
              name,
              round(OR[i], 2),
              round(CI_low[i], 2),
              round(CI_hi[i], 2),
              round(pvals[i], 4),
              sig))
}

# Build a results table for the app
results_table <- data.frame(
  Predictor = names(fit$coefficients),
  OR        = round(OR, 2),
  CI_low    = round(CI_low, 2),
  CI_high   = round(CI_hi, 2),
  p_value   = round(pvals, 4)
)

# Remove intercept row from table (not clinically meaningful)
results_table <- results_table[results_table$Predictor != "(Intercept)", ]

register_output(
  "Logistic regression",
  results_table,
  "table",
  paste("Logistic regression predicting patient satisfaction  N =", fit$N)
)

cat("Done\n")
