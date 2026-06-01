# Tutorial 2: Compare a number between two groups
#
# You have patients split into two groups (e.g. diagnosis type)
# and you want to know if a measurement differs between them.
# This uses a Welch t-test, the standard way to compare two groups.

library(fedstats)

ANALYSIS_TITLE <- "Group Comparison"

VARS_SPEC <- list(
  age      = list(type = "numeric", min = 18, max = 100),
  bmi      = list(type = "numeric", min = 10, max = 80),
  diag_grp = list(type = "categorical",
                  levels = c("radiculopathy", "myelopathy"))
)

# Leave this block as it is
if (!exists("servers", inherits = FALSE)) {
  .csvs <- sort(list.files("data", pattern = "[.]csv$", full.names = TRUE))
  if (!length(.csvs)) stop("No CSV files found in the data folder")
  servers <- lapply(.csvs, create_server)
}

clear_outputs()

# Compare age between the two diagnosis groups
# fed_welch_t needs: servers, variable, grouping variable, group 1, group 2
result <- fed_welch_t(servers, "age", "diag_grp", "radiculopathy", "myelopathy")

# Print the result to the console
cat("Radiculopathy patients\n")
cat("  Average age:", round(result$mean1, 1), "\n")
cat("  n =", result$n1, "\n")

cat("Myelopathy patients\n")
cat("  Average age:", round(result$mean2, 1), "\n")
cat("  n =", result$n2, "\n")

cat("p-value:", round(result$p, 4), "\n")

# p < 0.05 means the difference is statistically significant
if (result$p < 0.05) {
  cat("The groups differ significantly in age\n")
} else {
  cat("No significant difference in age between groups\n")
}

# Also compare BMI
result_bmi <- fed_welch_t(servers, "bmi", "diag_grp", "radiculopathy", "myelopathy")

# Build a results table
comparison_table <- data.frame(
  Variable       = c("Age (years)", "BMI"),
  Radiculopathy  = c(
    paste0(round(result$mean1, 1), " (n=", result$n1, ")"),
    paste0(round(result_bmi$mean1, 1), " (n=", result_bmi$n1, ")")
  ),
  Myelopathy     = c(
    paste0(round(result$mean2, 1), " (n=", result$n2, ")"),
    paste0(round(result_bmi$mean2, 1), " (n=", result_bmi$n2, ")")
  ),
  p_value        = c(round(result$p, 4), round(result_bmi$p, 4))
)

register_output(
  "Group comparison",
  comparison_table,
  "table",
  "Welch t-test comparing radiculopathy vs myelopathy patients"
)

cat("Done\n")
