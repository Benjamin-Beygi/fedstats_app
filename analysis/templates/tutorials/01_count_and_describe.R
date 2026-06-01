# Tutorial 1: Count patients and describe your data
#
# This is the simplest possible analysis.
# You ask each site for basic numbers about one variable
# and the package combines the answers for you.
#
# Every analysis file needs these four things:
#   1. library(fedstats)
#   2. ANALYSIS_TITLE
#   3. VARS_SPEC
#   4. register_output() at the end to show results in the app

library(fedstats)

# The title shown in the app
ANALYSIS_TITLE <- "My First Analysis"

# Tell the app which variables you will use and what type they are
# numeric  = a number like age or weight
# binary   = yes or no, coded as 1 or 0 in the data
VARS_SPEC <- list(
  age  = list(type = "numeric", min = 18, max = 100),
  bmi  = list(type = "numeric", min = 10, max = 80),
  sexM = list(type = "binary")
)

# Leave this block exactly as it is
# It connects to the sites (or loads CSV files for testing)
if (!exists("servers", inherits = FALSE)) {
  .csvs <- sort(list.files("data", pattern = "[.]csv$", full.names = TRUE))
  if (!length(.csvs)) stop("No CSV files found in the data folder")
  servers <- lapply(.csvs, create_server)
}

clear_outputs()

# Ask all sites for age information
# The result has: $n (count) $mean (average) $sd (spread)
age <- fed_numeric(servers, "age")

cat("Total patients:", age$n, "\n")
cat("Average age:", round(age$mean, 1), "\n")
cat("Age spread (SD):", round(age$sd, 1), "\n")

# Ask for BMI
bmi <- fed_numeric(servers, "bmi")

cat("Average BMI:", round(bmi$mean, 1), "\n")

# Ask for sex (binary variable)
# $sum = number of males (value = 1)
# $mean = proportion of males
sex <- fed_numeric(servers, "sexM")

cat("Male patients:", sex$sum, "out of", sex$n, "\n")
cat("Percent male:", round(100 * sex$mean, 1), "%\n")

# Build a simple summary table and show it in the app
summary_table <- data.frame(
  Variable = c("Age (years)", "BMI", "Male sex"),
  N        = c(age$n, bmi$n, sex$n),
  Result   = c(
    paste0(round(age$mean, 1), " (SD ", round(age$sd, 1), ")"),
    paste0(round(bmi$mean, 1), " (SD ", round(bmi$sd, 1), ")"),
    paste0(sex$sum, " (", round(100 * sex$mean, 0), "%)")
  )
)

# register_output sends the table to a tab in the app
# arguments: tab name, the data, type, description
register_output(
  "Summary",
  summary_table,
  "table",
  "Basic patient description"
)

cat("Done\n")
