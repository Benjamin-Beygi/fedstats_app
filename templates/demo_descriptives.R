# templates/demo_descriptives.R
# =====================================================================
# TEMPLATE: Federated Descriptive Statistics
#
# Demonstrates fed_numeric() and fed_validate() to compute pooled
# means, SDs, counts, and proportions from distributed site data.
#
# VARIABLES USED (must exist in each site's CSV):
#   age, bmi, sexM, smoker, nrs_arm_preop, eq5d_index_preop,
#   ndi_index_preop, diag_grp
#
# STANDALONE:  Rscript templates/demo_descriptives.R
# GUI:         Load this file in the coordinator app
# =====================================================================

library(fedstats)

ANALYSIS_TITLE <- "Federated Descriptives — Baseline Characteristics"

VARS_SPEC <- list(
  age            = list(type = "numeric", min = 18, max = 100),
  bmi            = list(type = "numeric", min = 10, max = 80),
  sexM           = list(type = "binary"),
  smoker         = list(type = "binary"),
  nrs_arm_preop  = list(type = "numeric", min = 0,  max = 10),
  eq5d_index_preop = list(type = "numeric", min = -1, max = 1),
  ndi_index_preop  = list(type = "numeric", min = 0,  max = 100),
  diag_grp       = list(type = "categorical",
                         levels = c("radiculopathy", "myelopathy"))
)

# =====================================================================
# Standalone server setup — skipped when GUI injects `servers`
# =====================================================================
if (!exists("servers", inherits = FALSE)) {
  .urls <- trimws(strsplit(Sys.getenv("FED_SITE_URLS", ""), ",")[[1]])
  .urls <- .urls[nzchar(.urls)]
  if (length(.urls) > 0) {
    .tok <- Sys.getenv("FED_API_TOKEN", "")
    servers <- lapply(.urls, function(u)
      create_remote_server(u, token = if (nzchar(.tok)) .tok else NULL))
  } else {
    .csvs <- sort(list.files("data", pattern = "[.]csv$", full.names = TRUE))
    if (!length(.csvs)) stop("No sites found. Add CSV files to ./data/ or set FED_SITE_URLS.")
    servers <- lapply(.csvs, create_server)
    cat(sprintf("Loaded %d local CSV site(s).\n\n", length(servers)))
  }
}

clear_outputs()

# =====================================================================
# Step 1: Validate data
# =====================================================================
val <- fed_validate(servers, VARS_SPEC)
if (!val$ok)
  warning("Validation issues detected — check Validation output tab.")

register_output("Validation", {
  lines <- c(
    sprintf("Sites: %d  |  Status: %s", length(val$site_reports),
            if (val$ok) "PASS" else "FAIL")
  )
  if (length(val$errors) > 0)
    lines <- c(lines, "Errors:", paste0("  ", val$errors))
  if (length(val$warnings) > 0)
    lines <- c(lines, "Warnings:", paste0("  ", val$warnings))
  paste(lines, collapse = "\n")
}, "text", "Pre-analysis validation report")

# =====================================================================
# Step 2: Compute summaries using fed_numeric()
#
# fed_numeric(servers, varname) returns:
#   $n      — total non-missing observations (pooled)
#   $mean   — pooled mean
#   $sd     — pooled standard deviation
#   $sum    — pooled sum  (use for binary variables: sum = number of 1s)
# =====================================================================

# --- Continuous variables ---
s_age  <- fed_numeric(servers, "age")
s_bmi  <- fed_numeric(servers, "bmi")
s_nrs  <- fed_numeric(servers, "nrs_arm_preop")
s_eq5d <- fed_numeric(servers, "eq5d_index_preop")
s_ndi  <- fed_numeric(servers, "ndi_index_preop")

# --- Binary variables (sum = count of 1s, mean = proportion) ---
s_sex    <- fed_numeric(servers, "sexM")
s_smoker <- fed_numeric(servers, "smoker")

cat(sprintf("%-20s  n = %5d  mean (SD) = %.2f (%.2f)\n",
            "Age (years):", s_age$n, s_age$mean, s_age$sd))
cat(sprintf("%-20s  n = %5d  mean (SD) = %.2f (%.2f)\n",
            "BMI (kg/m²):", s_bmi$n, s_bmi$mean, s_bmi$sd))
cat(sprintf("%-20s  n = %5d  mean (SD) = %.2f (%.2f)\n",
            "NRS arm preop:", s_nrs$n, s_nrs$mean, s_nrs$sd))
cat(sprintf("%-20s  n = %5d  %.0f / %d (%.1f%%)\n",
            "Male sex:", s_sex$n, s_sex$sum, s_sex$n, 100 * s_sex$mean))
cat(sprintf("%-20s  n = %5d  %.0f / %d (%.1f%%)\n",
            "Smoker:", s_smoker$n, s_smoker$sum, s_smoker$n, 100 * s_smoker$mean))

# =====================================================================
# Step 3: Per-group summaries using fed_group_numeric()
#
# fed_group_numeric(servers, varname, groupvar) returns a named list
# keyed by group level. Each entry has $n, $mean, $sd.
# =====================================================================
grp <- fed_group_numeric(servers, "nrs_arm_preop", "diag_grp")

cat("\nNRS arm preop by diagnosis:\n")
for (g in names(grp)) {
  cat(sprintf("  %-20s  n = %5d  mean = %.2f  SD = %.2f\n",
              g, grp[[g]]$n, grp[[g]]$mean, grp[[g]]$sd))
}

# =====================================================================
# Step 4: Build a Table 1 data frame and register for the GUI
# =====================================================================
tbl1 <- data.frame(
  Variable = c(
    "Age (years)",    "BMI (kg/m²)",
    "Male sex",       "Smoker",
    "NRS arm — preop", "EQ-5D index — preop", "NDI index — preop"
  ),
  N = c(s_age$n, s_bmi$n, s_sex$n, s_smoker$n, s_nrs$n, s_eq5d$n, s_ndi$n),
  "Mean (SD) or n (%)" = c(
    sprintf("%.2f (%.2f)", s_age$mean,  s_age$sd),
    sprintf("%.2f (%.2f)", s_bmi$mean,  s_bmi$sd),
    sprintf("%.0f (%.1f%%)", s_sex$sum,    100 * s_sex$mean),
    sprintf("%.0f (%.1f%%)", s_smoker$sum, 100 * s_smoker$mean),
    sprintf("%.2f (%.2f)", s_nrs$mean,  s_nrs$sd),
    sprintf("%.2f (%.2f)", s_eq5d$mean, s_eq5d$sd),
    sprintf("%.2f (%.2f)", s_ndi$mean,  s_ndi$sd)
  ),
  check.names = FALSE, stringsAsFactors = FALSE
)

register_output("Descriptives", tbl1, "table",
                "Baseline characteristics — pooled across all federated sites")

cat("\nDone. Outputs registered:", length(get_outputs()), "\n")
