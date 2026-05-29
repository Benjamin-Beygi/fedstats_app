# templates/demo_welch_t.R
# =====================================================================
# TEMPLATE: Federated Welch t-test
#
# Demonstrates fed_welch_t() to compare a continuous variable between
# two groups across federated sites. The test is exact: pooling the
# per-site sufficient statistics (n, sum, sum-of-squares per group)
# yields the same result as centralised Welch t-test.
#
# VARIABLES USED: age, nrs_arm_preop, eq5d_index_preop, diag_grp,
#                 diag_myelo, sexM
#
# STANDALONE:  Rscript templates/demo_welch_t.R
# GUI:         Load this file in the coordinator app
# =====================================================================

library(fedstats)

ANALYSIS_TITLE <- "Federated Welch t-test — Group Comparisons"

VARS_SPEC <- list(
  age              = list(type = "numeric", min = 18, max = 100),
  nrs_arm_preop    = list(type = "numeric", min = 0,  max = 10),
  nrs_neck_preop   = list(type = "numeric", min = 0,  max = 10),
  eq5d_index_preop = list(type = "numeric", min = -1, max = 1),
  ndi_index_preop  = list(type = "numeric", min = 0,  max = 100),
  bmi              = list(type = "numeric", min = 10, max = 80),
  diag_grp         = list(type = "categorical",
                           levels = c("radiculopathy", "myelopathy")),
  sexM             = list(type = "binary")
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
# fed_welch_t(servers, varname, groupvar, level1, level2)
#
# Arguments:
#   servers  — list of server objects
#   varname  — the continuous outcome variable
#   groupvar — the binary or categorical grouping variable
#   level1   — character label for group 1
#   level2   — character label for group 2
#
# Returns list with: mean1, mean2, sd1, sd2, n1, n2, t, df, p
# =====================================================================

# ---- Example 1: age by diagnosis group ----
cat("=== Example 1: Age by diagnosis group ===\n")

tt1 <- fed_welch_t(servers, "age", "diag_grp",
                   "radiculopathy", "myelopathy")

cat(sprintf("  Radiculopathy:  mean = %.2f  SD = %.2f  n = %d\n",
            tt1$mean1, tt1$sd1, tt1$n1))
cat(sprintf("  Myelopathy:     mean = %.2f  SD = %.2f  n = %d\n",
            tt1$mean2, tt1$sd2, tt1$n2))
cat(sprintf("  t(%.1f) = %.4f,  p = %.4g\n\n", tt1$df, tt1$t, tt1$p))

# ---- Example 2: NRS arm by sex ----
cat("=== Example 2: NRS arm preop by sex ===\n")

# When groupvar is binary (0/1), use "0" and "1" as level strings
tt2 <- fed_welch_t(servers, "nrs_arm_preop", "sexM", "0", "1")

cat(sprintf("  Female (sexM=0): mean = %.2f  SD = %.2f  n = %d\n",
            tt2$mean1, tt2$sd1, tt2$n1))
cat(sprintf("  Male   (sexM=1): mean = %.2f  SD = %.2f  n = %d\n",
            tt2$mean2, tt2$sd2, tt2$n2))
cat(sprintf("  t(%.1f) = %.4f,  p = %.4g\n\n", tt2$df, tt2$t, tt2$p))

# =====================================================================
# Run multiple comparisons and build a summary table
# =====================================================================
.comparisons <- list(
  list(var = "age",              label = "Age (years)"),
  list(var = "bmi",              label = "BMI (kg/m²)"),
  list(var = "nrs_arm_preop",    label = "NRS arm — preop"),
  list(var = "nrs_neck_preop",   label = "NRS neck — preop"),
  list(var = "eq5d_index_preop", label = "EQ-5D index — preop"),
  list(var = "ndi_index_preop",  label = "NDI index — preop")
)

rows <- lapply(.comparisons, function(item) {
  tt <- fed_welch_t(servers, item$var, "diag_grp",
                    "radiculopathy", "myelopathy")
  data.frame(
    Variable = item$label,
    "Radiculopathy" = sprintf("%.2f ± %.2f (n=%d)", tt$mean1, tt$sd1, tt$n1),
    "Myelopathy"    = sprintf("%.2f ± %.2f (n=%d)", tt$mean2, tt$sd2, tt$n2),
    t  = round(tt$t,  3),
    df = round(tt$df, 1),
    p  = round(tt$p,  4),
    check.names = FALSE, stringsAsFactors = FALSE
  )
})

result_tab <- do.call(rbind, rows)

register_output(
  "Group comparisons",
  result_tab,
  "table",
  "Welch t-test: continuous variables by diagnosis (radiculopathy vs myelopathy)"
)

# ---- Forest-style dot-plot of mean differences ----
register_output(
  "Mean difference plot",
  value = local({
    .rows <- rows  # captured list of result lists
    .comps <- .comparisons
    function() {
      # Recompute differences for plotting
      diffs <- vapply(.rows, function(r) as.numeric(strsplit(r[[2]], " ")[[1]][1]) -
                                          as.numeric(strsplit(r[[3]], " ")[[1]][1]), numeric(1))
      n_items <- length(diffs)
      y_pos <- seq(n_items, 1)
      par(mar = c(4.5, 11, 3.5, 2))
      plot(diffs, y_pos,
           xlab = "Mean difference (Radiculopathy − Myelopathy)",
           ylab = "", yaxt = "n",
           pch = 18, cex = 1.6, col = "steelblue4",
           xlim = c(min(diffs) * 1.4, max(diffs) * 1.4),
           ylim = c(0.3, n_items + 0.7),
           main = "Mean differences by diagnosis group")
      abline(v = 0, lty = 2, col = "grey55")
      axis(2, at = y_pos,
           labels = vapply(.comps, function(x) x$label, character(1)),
           las = 1, cex.axis = 0.85)
      text(diffs, y_pos + 0.38,
           labels = sprintf("%.2f", diffs), cex = 0.78, col = "steelblue4")
    }
  }),
  type    = "plot",
  caption = "Mean difference (radiculopathy − myelopathy) for each continuous variable"
)

cat("Done. Outputs registered:", length(get_outputs()), "\n")
