# Tutorial 5: Are two yes/no variables related? (chi-square test)
#
# Use this when both variables are yes/no (binary).
# For example: do smokers less often achieve meaningful pain improvement?
#
# The chi-square test tells you whether the two variables are associated
# or whether any difference is likely just random chance.
#
# p < 0.05 means the association is statistically significant.

library(fedstats)

ANALYSIS_TITLE <- "Chi-Square Test"

VARS_SPEC <- list(
  smoker                = list(type = "binary"),
  mcid_arm_12m          = list(type = "binary"),
  sexM                  = list(type = "binary"),
  patient_satisfied_12m = list(type = "binary")
)

# Leave this block as it is
if (!exists("servers", inherits = FALSE)) {
  .csvs <- sort(list.files("data", pattern = "[.]csv$", full.names = TRUE))
  if (!length(.csvs)) stop("No CSV files found in the data folder")
  servers <- lapply(.csvs, create_server)
}

clear_outputs()

# Test 1: Is smoking associated with achieving meaningful pain improvement?
# fed_chisq_2x2 needs: servers, first variable, second variable
result <- fed_chisq_2x2(servers, "smoker", "mcid_arm_12m")

# Print the 2x2 table (counts of each combination)
cat("Patients included:", result$N, "\n\n")
cat("Observed counts\n")
print(result$table)
cat("\n")

# Print the test result
cat(sprintf("Chi-square = %.2f  df = %d  p = %.4f\n",
            result$statistic, result$df, result$p))

if (result$p < 0.05) {
  cat("Smoking and pain improvement are significantly associated\n")
} else {
  cat("No significant association between smoking and pain improvement\n")
}

cat("\n")

# Test 2: Is sex associated with patient satisfaction?
result2 <- fed_chisq_2x2(servers, "sexM", "patient_satisfied_12m")

cat("Observed counts\n")
print(result2$table)
cat("\n")
cat(sprintf("Chi-square = %.2f  df = %d  p = %.4f\n",
            result2$statistic, result2$df, result2$p))

if (result2$p < 0.05) {
  cat("Sex and patient satisfaction are significantly associated\n")
} else {
  cat("No significant association between sex and patient satisfaction\n")
}

# Build a results table
results_table <- data.frame(
  Variable_1  = c("smoker",  "sexM"),
  Variable_2  = c("mcid_arm_12m", "patient_satisfied_12m"),
  Chi_square  = round(c(result$statistic,  result2$statistic), 2),
  df          = c(result$df,  result2$df),
  p_value     = round(c(result$p, result2$p), 4),
  N           = c(result$N,  result2$N)
)

register_output(
  "Chi-square results",
  results_table,
  "table",
  "Chi-square tests for association between binary variables"
)

cat("Done\n")
