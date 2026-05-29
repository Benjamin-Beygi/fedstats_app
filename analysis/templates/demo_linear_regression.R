# templates/demo_linear_regression.R
# ─────────────────────────────────────────────────────────────────────
# Federated linear regression (OLS)
#
# Predicts how much a continuous outcome (e.g. pain score) changes
# from baseline to 12 months, given patient characteristics.
# Each site shares only compact summary matrices — no patient rows leave
# the site. The coordinator combines them and fits the model exactly as
# R's lm() would on the combined dataset.
#
# Run standalone:  Rscript templates/demo_linear_regression.R
# Run via GUI:     load this file in the coordinator app
# ─────────────────────────────────────────────────────────────────────

library(fedstats)

ANALYSIS_TITLE <- "Federated Linear Regression — 12-month Outcomes"

# ── ADAPT: list every variable the formula uses ──────────────────────
# delta_* variables are the change from baseline to 12 months
# (positive = improvement for NRS/NDI; positive = improvement for EQ-5D too)
VARS_SPEC <- list(
  delta_nrs_arm_12m  = list(type = "numeric", min = -10,  max = 10),
  delta_eq5d_12m     = list(type = "numeric", min = -2,   max = 2),
  delta_ndi_12m      = list(type = "numeric", min = -100, max = 100),
  age                = list(type = "numeric", min = 18,   max = 100),
  sexM               = list(type = "binary"),
  bmi                = list(type = "numeric", min = 10,   max = 80),
  smoker             = list(type = "binary"),
  nrs_arm_preop      = list(type = "numeric", min = 0,    max = 10),
  eq5d_index_preop   = list(type = "numeric", min = -1,   max = 1),
  ndi_index_preop    = list(type = "numeric", min = 0,    max = 100),
  diag_myelo         = list(type = "binary"),
  asa                = list(type = "numeric", min = 1,    max = 5),
  opioids_preop      = list(type = "binary"),
  comorbid_cardiac   = list(type = "binary")
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
# fed_lm(servers, formula)
#
# formula — standard R formula, e.g.  outcome ~ x1 + x2
# Returns: $coefficients, $se, $t, $p, $N, $df, $sigma (residual SD)
#
# Interpretation: Estimate > 0 means the predictor is associated with
# greater improvement in the outcome (for delta variables where a
# higher value means more improvement). For NRS and NDI, lower scores
# are better so a negative delta means pain got worse.
# ─────────────────────────────────────────────────────────────────────

# Helper: turn a fitted model into a tidy table
.make_lm_tab <- function(fit) {
  tab <- data.frame(
    Term      = names(fit$coefficients),
    Estimate  = round(fit$coefficients, 3),
    Std.Error = round(fit$se,           3),
    t.value   = round(fit$t,            3),
    p.value   = round(fit$p,            4),
    stringsAsFactors = FALSE
  )
  rownames(tab) <- NULL
  tab
}

# ─────────────────────────────────────────────────────────────────────
# Model 1: Did arm pain improve? — NRS arm change at 12 months
# ── ADAPT: change the outcome variable and predictors to match your study
# ─────────────────────────────────────────────────────────────────────
cat("=== Model 1: NRS arm change at 12m ===\n")

m1 <- fed_lm(
  servers,
  delta_nrs_arm_12m ~ age + sexM + bmi + smoker +
    nrs_arm_preop + diag_myelo + asa + opioids_preop
)

m1_tab <- .make_lm_tab(m1)
cat(sprintf("  N = %d  |  Residual SE = %.3f  |  df = %d\n",
            m1$N, m1$sigma, m1$df))
cat(sprintf("  nrs_arm_preop:  β = %.3f  (SE %.3f)  p = %.4f\n\n",
            m1$coefficients["nrs_arm_preop"],
            m1$se["nrs_arm_preop"],
            m1$p["nrs_arm_preop"]))

register_output(
  "Model 1 — NRS arm change",
  m1_tab,
  "table",
  sprintf("OLS: delta_nrs_arm_12m | N = %d | Residual SE = %.3f", m1$N, m1$sigma)
)

# ─────────────────────────────────────────────────────────────────────
# Model 2: Did quality of life improve? — EQ-5D change at 12 months
# ── ADAPT: adjust predictors as needed
# ─────────────────────────────────────────────────────────────────────
cat("=== Model 2: EQ-5D change at 12m ===\n")

m2 <- fed_lm(
  servers,
  delta_eq5d_12m ~ age + sexM + bmi + smoker +
    eq5d_index_preop + diag_myelo + asa + comorbid_cardiac
)

m2_tab <- .make_lm_tab(m2)
cat(sprintf("  N = %d  |  Residual SE = %.3f  |  df = %d\n",
            m2$N, m2$sigma, m2$df))
cat(sprintf("  eq5d_index_preop:  β = %.3f  (SE %.3f)  p = %.4f\n\n",
            m2$coefficients["eq5d_index_preop"],
            m2$se["eq5d_index_preop"],
            m2$p["eq5d_index_preop"]))

register_output(
  "Model 2 — EQ-5D change",
  m2_tab,
  "table",
  sprintf("OLS: delta_eq5d_12m | N = %d | Residual SE = %.3f", m2$N, m2$sigma)
)

# ─────────────────────────────────────────────────────────────────────
# Model 3: Did neck disability improve? — NDI change at 12 months
# ── ADAPT: adjust predictors as needed
# ─────────────────────────────────────────────────────────────────────
cat("=== Model 3: NDI change at 12m ===\n")

m3 <- fed_lm(
  servers,
  delta_ndi_12m ~ age + sexM + bmi + smoker +
    ndi_index_preop + diag_myelo + asa + opioids_preop
)

m3_tab <- .make_lm_tab(m3)
cat(sprintf("  N = %d  |  Residual SE = %.3f  |  df = %d\n\n",
            m3$N, m3$sigma, m3$df))

register_output(
  "Model 3 — NDI change",
  m3_tab,
  "table",
  sprintf("OLS: delta_ndi_12m | N = %d | Residual SE = %.3f", m3$N, m3$sigma)
)

# ── Coefficient comparison plot across all three models ───────────────
# Shows estimates for predictors that appear in all three models,
# so you can visually compare whether a variable (e.g. age, sex) has
# a consistent direction of effect across different outcomes.
register_output(
  "Coefficient plot (3 models)",
  value = local({
    .m1 <- m1_tab; .m2 <- m2_tab; .m3 <- m3_tab
    function() {
      .common <- intersect(
        intersect(.m1$Term[.m1$Term != "(Intercept)"],
                  .m2$Term[.m2$Term != "(Intercept)"]),
        .m3$Term[.m3$Term != "(Intercept)"]
      )
      if (!length(.common)) { plot.new(); text(0.5, 0.5, "No common terms."); return() }
      .n    <- length(.common)
      .y    <- seq(.n, 1)
      .cols <- c("#6A0DAD", "#FF1493", "#2b6cb0")   # KM purple, pink, blue
      .labs <- c("NRS arm Δ", "EQ-5D Δ", "NDI Δ")
      .all_est <- c(.m1$Estimate, .m2$Estimate, .m3$Estimate)
      .xlim <- c(min(.all_est) * 1.4, max(.all_est) * 1.4)
      par(mar = c(4.5, 9, 3.5, 1.5))
      plot(NULL, xlim = .xlim, ylim = c(0.3, .n + 1.4),
           xlab = "Coefficient estimate (±95% CI)", ylab = "",
           yaxt = "n", main = "Regression coefficients — 3 outcome models")
      abline(v = 0, lty = 2, col = "grey65")
      for (.mi in 1:3) {
        .tab <- list(.m1, .m2, .m3)[[.mi]]
        .r   <- .tab[match(.common, .tab$Term), ]
        .off <- (.mi - 2) * 0.2
        points(.r$Estimate, .y + .off, pch = 14 + .mi,
               cex = 1.3, col = .cols[.mi])
        for (.j in seq_len(.n))
          lines(c(.r$Estimate[.j] - 1.96 * .r$Std.Error[.j],
                  .r$Estimate[.j] + 1.96 * .r$Std.Error[.j]),
                c(.y[.j] + .off, .y[.j] + .off),
                col = .cols[.mi], lwd = 1.8)
      }
      axis(2, at = .y, labels = .common, las = 1, cex.axis = 0.85)
      legend("topright", legend = .labs, col = .cols,
             pch = 15:17, cex = 0.82, bty = "n")
    }
  }),
  type    = "plot",
  caption = "OLS coefficient estimates (±95% CI) for shared predictors across three outcome models. Dashed line = no effect."
)

cat("Done. Outputs registered:", length(get_outputs()), "\n")
