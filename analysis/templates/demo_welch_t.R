# templates/demo_welch_t.R
# ─────────────────────────────────────────────────────────────────────
# Federated Welch t-test — compare a continuous variable between groups
#
# Each site returns per-group counts, sums, and sums-of-squares.
# The coordinator pools these to compute the exact Welch t-statistic —
# numerically identical to a centralised test on the combined dataset.
#
# Run standalone:  Rscript templates/demo_welch_t.R
# Run via GUI:     load this file in the coordinator app
# ─────────────────────────────────────────────────────────────────────

library(fedstats)

ANALYSIS_TITLE <- "Federated Welch t-test — Group Comparisons"

# ── ADAPT: list every variable used in the comparisons below ─────────
VARS_SPEC <- list(
  age              = list(type = "numeric", min = 18,  max = 100),
  bmi              = list(type = "numeric", min = 10,  max = 80),
  nrs_arm_preop    = list(type = "numeric", min = 0,   max = 10),
  nrs_neck_preop   = list(type = "numeric", min = 0,   max = 10),
  eq5d_index_preop = list(type = "numeric", min = -1,  max = 1),
  ndi_index_preop  = list(type = "numeric", min = 0,   max = 100),
  diag_grp         = list(type = "categorical",
                           levels = c("radiculopathy", "myelopathy")),
  sexM             = list(type = "binary")
)

# ── Server setup ─────────────────────────────────────────────────────
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

# ─────────────────────────────────────────────────────────────────────
# fed_welch_t(servers, varname, groupvar, level1, level2)
#
# varname  — the continuous variable to compare (e.g. "age")
# groupvar — the grouping variable                (e.g. "diag_grp")
# level1   — exact string value of group 1        (e.g. "radiculopathy")
# level2   — exact string value of group 2        (e.g. "myelopathy")
#
# For binary (0/1) variables, use "0" and "1" as the level strings.
#
# Returns: mean1, mean2, sd1, sd2, n1, n2, t-statistic, df, p-value
# ─────────────────────────────────────────────────────────────────────

# ── Example 1: age by diagnosis ──────────────────────────────────────
# ── ADAPT: change varname and levels to match your comparison ────────
cat("=== Example 1: Age by diagnosis group ===\n")

tt1 <- fed_welch_t(servers, "age", "diag_grp", "radiculopathy", "myelopathy")

cat(sprintf("  Radiculopathy:  mean = %.1f  SD = %.1f  n = %d\n",
            tt1$mean1, tt1$sd1, tt1$n1))
cat(sprintf("  Myelopathy:     mean = %.1f  SD = %.1f  n = %d\n",
            tt1$mean2, tt1$sd2, tt1$n2))
cat(sprintf("  t(%.0f) = %.3f,  p = %.4f\n\n", tt1$df, tt1$t, tt1$p))

# ── Example 2: NRS arm by sex ────────────────────────────────────────
# When the groupvar is a binary 0/1 column, pass "0" and "1" as strings.
cat("=== Example 2: NRS arm preop by sex ===\n")

tt2 <- fed_welch_t(servers, "nrs_arm_preop", "sexM", "0", "1")

cat(sprintf("  Female (sexM=0): mean = %.1f  SD = %.1f  n = %d\n",
            tt2$mean1, tt2$sd1, tt2$n1))
cat(sprintf("  Male   (sexM=1): mean = %.1f  SD = %.1f  n = %d\n",
            tt2$mean2, tt2$sd2, tt2$n2))
cat(sprintf("  t(%.0f) = %.3f,  p = %.4f\n\n", tt2$df, tt2$t, tt2$p))

# ── Batch comparisons: build a summary table ─────────────────────────
# ── ADAPT: add/remove rows to match the variables you want to compare ─
.comparisons <- list(
  list(var = "age",              label = "Age (years)"),
  list(var = "bmi",              label = "BMI (kg/m²)"),
  list(var = "nrs_arm_preop",    label = "NRS arm — preop"),
  list(var = "nrs_neck_preop",   label = "NRS neck — preop"),
  list(var = "eq5d_index_preop", label = "EQ-5D index — preop"),
  list(var = "ndi_index_preop",  label = "NDI index — preop")
)

rows <- lapply(.comparisons, function(item) {
  tt <- fed_welch_t(servers, item$var, "diag_grp", "radiculopathy", "myelopathy")
  data.frame(
    Variable        = item$label,
    "Radiculopathy" = sprintf("%.1f ± %.1f  (n=%d)", tt$mean1, tt$sd1, tt$n1),
    "Myelopathy"    = sprintf("%.1f ± %.1f  (n=%d)", tt$mean2, tt$sd2, tt$n2),
    t               = round(tt$t,  3),
    df              = round(tt$df, 0),
    p               = round(tt$p,  4),
    check.names = FALSE, stringsAsFactors = FALSE
  )
})

register_output(
  "Group comparisons",
  do.call(rbind, rows),
  "table",
  "Welch t-test: continuous baseline variables by diagnosis"
)

# ── Mean-difference dot plot ──────────────────────────────────────────
register_output(
  "Mean difference plot",
  value = local({
    .rows  <- rows
    .comps <- .comparisons
    function() {
      # Extract group means and compute difference (radiculopathy − myelopathy)
      get_mean <- function(cell) as.numeric(strsplit(trimws(cell), " ")[[1]][1])
      diffs <- vapply(.rows, function(r) get_mean(r[[2]]) - get_mean(r[[3]]), numeric(1))
      labels <- vapply(.comps, function(x) x$label, character(1))

      n_items <- length(diffs)
      y_pos   <- seq(n_items, 1)
      par(mar = c(4.5, 11, 3.5, 2))
      plot(diffs, y_pos,
           xlab = "Mean difference (Radiculopathy − Myelopathy)",
           ylab = "", yaxt = "n",
           pch = 18, cex = 1.6,
           col = "#6A0DAD",   # Karolinska purple
           xlim = c(min(diffs) * 1.5, max(diffs) * 1.5),
           ylim = c(0.3, n_items + 0.7),
           main = "Mean differences by diagnosis group")
      abline(v = 0, lty = 2, col = "grey55")
      axis(2, at = y_pos, labels = labels, las = 1, cex.axis = 0.85)
      text(diffs, y_pos + 0.38,
           labels = sprintf("%.2f", diffs), cex = 0.78, col = "#4a0080")
    }
  }),
  type    = "plot",
  caption = "Mean difference (radiculopathy − myelopathy). Dashed line = no difference."
)

cat("Done. Outputs registered:", length(get_outputs()), "\n")
