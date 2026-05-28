# templates/demo_chisq.R
# =====================================================================
# TEMPLATE: Federated Chi-square test (2×2)
#
# Demonstrates fed_chisq_2x2() to test association between two binary
# variables across federated sites. Each site contributes its 2×2
# contingency counts; these are pooled exactly before computing X².
#
# VARIABLES USED: sexM, diag_myelo, smoker, mcid_arm_12m,
#                 any_complication, opioids_preop, patient_satisfied_12m
#
# STANDALONE:  Rscript templates/demo_chisq.R
# GUI:         Load this file in the coordinator app
# =====================================================================

library(fedstats)

ANALYSIS_TITLE <- "Federated Chi-square — Binary Associations"

VARS_SPEC <- list(
  sexM                  = list(type = "binary"),
  diag_myelo            = list(type = "binary"),
  smoker                = list(type = "binary"),
  opioids_preop         = list(type = "binary"),
  comorbid_cardiac      = list(type = "binary"),
  mcid_arm_12m          = list(type = "binary"),
  mcid_eq5d_12m         = list(type = "binary"),
  patient_satisfied_12m = list(type = "binary"),
  any_complication      = list(type = "binary"),
  reoperated            = list(type = "binary")
)

# =====================================================================
# Standalone server setup
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
    if (!length(.csvs)) stop("No sites found.")
    servers <- lapply(.csvs, create_server)
    cat(sprintf("Loaded %d local CSV site(s).\n\n", length(servers)))
  }
}

clear_outputs()

# =====================================================================
# fed_chisq_2x2(servers, xvar, yvar, correct = TRUE)
#
# Arguments:
#   servers  — list of server objects
#   xvar     — first binary variable (rows)
#   yvar     — second binary variable (columns)
#   correct  — apply Yates continuity correction (default TRUE)
#
# Returns list with:
#   $table     — 2×2 frequency matrix (named rows and columns)
#   $statistic — chi-square test statistic
#   $df        — degrees of freedom (always 1 for 2×2)
#   $p         — p-value
#   $N         — total non-missing observations
# =====================================================================

# ---- Example 1: sex vs diagnosis ----
cat("=== Example 1: Sex vs Diagnosis ===\n")

chi1 <- fed_chisq_2x2(servers, "sexM", "diag_myelo", correct = TRUE)
print(chi1$table)
cat(sprintf("  X²(df=%d) = %.3f,  p = %.4g,  N = %d\n\n",
            chi1$df, chi1$statistic, chi1$p, chi1$N))

# ---- Example 2: smoker vs MCID arm ----
cat("=== Example 2: Smoker vs MCID arm (12m) ===\n")

chi2 <- fed_chisq_2x2(servers, "smoker", "mcid_arm_12m", correct = TRUE)
print(chi2$table)
cat(sprintf("  X²(df=%d) = %.3f,  p = %.4g,  N = %d\n\n",
            chi2$df, chi2$statistic, chi2$p, chi2$N))

# =====================================================================
# Batch: multiple binary outcomes vs diagnosis
# =====================================================================
.pairs <- list(
  list(x = "sexM",               label = "Male sex"),
  list(x = "smoker",             label = "Smoker"),
  list(x = "comorbid_cardiac",   label = "Cardiac comorbidity"),
  list(x = "opioids_preop",      label = "Opioid use preop"),
  list(x = "mcid_arm_12m",       label = "MCID arm NRS (12m)"),
  list(x = "mcid_eq5d_12m",      label = "MCID EQ-5D (12m)"),
  list(x = "patient_satisfied_12m", label = "Patient satisfied (12m)"),
  list(x = "any_complication",   label = "Any complication"),
  list(x = "reoperated",         label = "Reoperated")
)

chi_rows <- lapply(.pairs, function(item) {
  res <- tryCatch(
    fed_chisq_2x2(servers, item$x, "diag_myelo", correct = TRUE),
    error = function(e) NULL
  )
  if (is.null(res))
    return(data.frame(Variable = item$label,
                      "Radiculopathy (n)" = NA, "Myelopathy (n)" = NA,
                      X2 = NA, p = NA, N = NA,
                      check.names = FALSE, stringsAsFactors = FALSE))

  # Row names are "<xvar>=0"/"<xvar>=1"; column names "<yvar>=0"/"<yvar>=1"
  rn  <- rownames(res$table)   # e.g. c("sexM=0", "sexM=1")
  cn  <- colnames(res$table)   # e.g. c("diag_myelo=0", "diag_myelo=1")
  n_rad <- res$table[rn[2], cn[1]]  # x=1, y(diag_myelo)=0 → radiculopathy
  n_mye <- res$table[rn[2], cn[2]]  # x=1, y(diag_myelo)=1 → myelopathy
  tot_rad <- res$table[rn[1], cn[1]] + res$table[rn[2], cn[1]]
  tot_mye <- res$table[rn[1], cn[2]] + res$table[rn[2], cn[2]]

  data.frame(
    Variable = item$label,
    "Radiculopathy" = sprintf("%d / %d (%.1f%%)",
                               n_rad, tot_rad, 100 * n_rad / tot_rad),
    "Myelopathy"    = sprintf("%d / %d (%.1f%%)",
                               n_mye, tot_mye, 100 * n_mye / tot_mye),
    X2 = round(res$statistic, 3),
    p  = round(res$p,         4),
    N  = res$N,
    check.names = FALSE, stringsAsFactors = FALSE
  )
})

chi_tab <- do.call(rbind, chi_rows)

register_output(
  "Chi-square results",
  chi_tab,
  "table",
  "Chi-square (Yates): binary variables by diagnosis (radiculopathy vs myelopathy)"
)

# ---- Also show the 2x2 table for sex × diagnosis ----
.m <- chi1$table
tbl_df <- data.frame(
  " "                   = rownames(.m),
  as.data.frame(.m, stringsAsFactors = FALSE),
  check.names      = FALSE,
  stringsAsFactors = FALSE
)
rownames(tbl_df) <- NULL

register_output(
  "2×2 table: sex × diagnosis",
  tbl_df,
  "table",
  sprintf("Contingency table: sexM × diag_myelo | X²(1) = %.3f, p = %.4g, N = %d",
          chi1$statistic, chi1$p, chi1$N)
)

cat("Done. Outputs registered:", length(get_outputs()), "\n")
