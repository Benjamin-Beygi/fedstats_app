# templates/demo_logistic_regression.R
# ─────────────────────────────────────────────────────────────────────
# Federated logistic regression
#
# Each site shares only gradient and Hessian information — no individual
# patient data ever leaves the site. The coordinator iterates to
# convergence exactly as a central glm() would.
#
# Run standalone:  Rscript templates/demo_logistic_regression.R
# Run via GUI:     load this file in the coordinator app
# ─────────────────────────────────────────────────────────────────────

library(fedstats)

ANALYSIS_TITLE <- "Federated Logistic Regression — MCID & Satisfaction"

# ── ADAPT: list every variable the formula uses ──────────────────────
# Each variable name must exactly match the column name in the site CSV.
VARS_SPEC <- list(
  mcid_arm_12m          = list(type = "binary"),
  mcid_eq5d_12m         = list(type = "binary"),
  patient_satisfied_12m = list(type = "binary"),
  age                   = list(type = "numeric", min = 18, max = 100),
  sexM                  = list(type = "binary"),
  bmi                   = list(type = "numeric", min = 10, max = 80),
  smoker                = list(type = "binary"),
  nrs_arm_preop         = list(type = "numeric", min = 0,  max = 10),
  eq5d_index_preop      = list(type = "numeric", min = -1, max = 1),
  ndi_index_preop       = list(type = "numeric", min = 0,  max = 100),
  diag_myelo            = list(type = "binary"),
  asa                   = list(type = "numeric", min = 1,  max = 5),
  opioids_preop         = list(type = "binary"),
  comorbid_cardiac      = list(type = "binary")
)

# ── Server setup ─────────────────────────────────────────────────────
# When running inside the coordinator GUI, `servers` is already created.
# When running standalone (Rscript), this block sets up local test sites
# from CSV files in the data/ folder (or remote sites via FED_SITE_URLS).
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
# Helper: build a results table from a fitted logistic model
#
# Shows log-odds, SE, z, p, and the exponentiated OR with 95% CI.
# Uses cluster-robust SE when >= 2 sites are available (recommended).
# ─────────────────────────────────────────────────────────────────────
.logit_table <- function(fit) {
  .se <- if (!is.null(fit$se_robust)) fit$se_robust else fit$se
  .z  <- fit$coefficients / .se
  .p  <- 2 * (1 - pnorm(abs(.z)))
  tab <- data.frame(
    Term     = names(fit$coefficients),
    OR       = round(exp(fit$coefficients),        2),
    CI.lower = round(exp(fit$coefficients - 1.96 * .se), 2),
    CI.upper = round(exp(fit$coefficients + 1.96 * .se), 2),
    p        = round(.p, 4),
    stringsAsFactors = FALSE
  )
  rownames(tab) <- NULL
  tab
}

# Helper: forest plot of ORs — stored as a function so the GUI renders it
.forest_fn <- function(tab, title) {
  local({
    .tab <- tab; .ttl <- title
    function() {
      .r   <- .tab[.tab$Term != "(Intercept)", ]
      .n   <- nrow(.r)
      if (.n == 0) { plot.new(); text(0.5, 0.5, "No predictors."); return() }
      .y   <- seq(.n, 1)
      .xlo <- max(0.05, min(.r$CI.lower, na.rm = TRUE) * 0.75)
      .xhi <- max(.r$CI.upper, na.rm = TRUE) * 1.4
      .old <- par(mar = c(4.5, 10, 3.5, 2.5))
      on.exit(par(.old), add = TRUE)
      plot(.r$OR, .y,
           xlim = c(.xlo, .xhi), ylim = c(0.3, .n + 0.7),
           xlab = "Odds ratio (95% CI)", ylab = "",
           yaxt = "n", log = "x", pch = 15, cex = 1.4,
           col = "#6A0DAD",   # Karolinska purple
           main = .ttl)
      for (.i in seq_len(.n))
        lines(c(.r$CI.lower[.i], .r$CI.upper[.i]),
              c(.y[.i], .y[.i]), col = "#6A0DAD", lwd = 2)
      abline(v = 1, lty = 2, col = "grey60")
      axis(2, at = .y, labels = .r$Term, las = 1, cex.axis = 0.85)
      for (.i in seq_len(.n))
        text(.r$OR[.i], .y[.i] + 0.38,
             sprintf("%.2f [%.2f–%.2f]",
                     .r$OR[.i], .r$CI.lower[.i], .r$CI.upper[.i]),
             cex = 0.72, col = "#4a0080")
    }
  })
}

# ─────────────────────────────────────────────────────────────────────
# Model A: MCID arm NRS — did the arm pain improve enough to matter?
# ── ADAPT: change the formula and outcome variable to match your study
# ─────────────────────────────────────────────────────────────────────
cat("=== Model A: MCID arm NRS at 12m ===\n")

fitA <- fed_logistic_newton(
  servers,
  mcid_arm_12m ~ age + sexM + bmi + smoker +
    nrs_arm_preop + diag_myelo + asa + opioids_preop,
  robust_cluster = TRUE,   # recommended when >= 2 sites
  verbose        = FALSE
)

tabA <- .logit_table(fitA)
cat(sprintf("  N = %d  |  converged: %s  |  log-likelihood: %.1f\n",
            fitA$N, fitA$converged, fitA$logLik))
# OR > 1 = higher odds of achieving MCID; OR < 1 = lower odds
cat(sprintf("  nrs_arm_preop:  OR = %.2f  [%.2f–%.2f]  p = %.4f\n\n",
            tabA$OR[tabA$Term == "nrs_arm_preop"],
            tabA$CI.lower[tabA$Term == "nrs_arm_preop"],
            tabA$CI.upper[tabA$Term == "nrs_arm_preop"],
            tabA$p[tabA$Term == "nrs_arm_preop"]))

register_output("Model A — MCID arm NRS", tabA, "table",
                sprintf("Logistic: mcid_arm_12m | N = %d | log-likelihood = %.1f",
                        fitA$N, fitA$logLik))
register_output("Forest A — MCID arm NRS",
                .forest_fn(tabA, "MCID arm NRS at 12m — Odds Ratios"),
                "plot",
                "OR (95% CI). Dashed line = no effect (OR = 1).")

# ─────────────────────────────────────────────────────────────────────
# Model B: Patient satisfaction at 12 months
# ── ADAPT: adjust predictors based on your research question
# ─────────────────────────────────────────────────────────────────────
cat("=== Model B: Patient satisfaction at 12m ===\n")

fitB <- fed_logistic_newton(
  servers,
  patient_satisfied_12m ~ age + sexM + bmi + smoker +
    nrs_arm_preop + eq5d_index_preop + ndi_index_preop +
    diag_myelo + comorbid_cardiac + opioids_preop,
  robust_cluster = TRUE,
  verbose        = FALSE
)

tabB <- .logit_table(fitB)
cat(sprintf("  N = %d  |  converged: %s  |  log-likelihood: %.1f\n",
            fitB$N, fitB$converged, fitB$logLik))
cat(sprintf("  eq5d_index_preop:  OR = %.2f  [%.2f–%.2f]  p = %.4f\n\n",
            tabB$OR[tabB$Term == "eq5d_index_preop"],
            tabB$CI.lower[tabB$Term == "eq5d_index_preop"],
            tabB$CI.upper[tabB$Term == "eq5d_index_preop"],
            tabB$p[tabB$Term == "eq5d_index_preop"]))

register_output("Model B — Satisfaction", tabB, "table",
                sprintf("Logistic: patient_satisfied_12m | N = %d", fitB$N))
register_output("Forest B — Satisfaction",
                .forest_fn(tabB, "Patient Satisfaction at 12m — Odds Ratios"),
                "plot",
                "OR (95% CI). Dashed line = no effect (OR = 1).")

# ─────────────────────────────────────────────────────────────────────
# Model C: MCID EQ-5D at 12 months
# ─────────────────────────────────────────────────────────────────────
cat("=== Model C: MCID EQ-5D at 12m ===\n")

fitC <- fed_logistic_newton(
  servers,
  mcid_eq5d_12m ~ age + sexM + eq5d_index_preop +
    diag_myelo + smoker + asa + opioids_preop,
  robust_cluster = TRUE,
  verbose        = FALSE
)

tabC <- .logit_table(fitC)
cat(sprintf("  N = %d  |  converged: %s  |  log-likelihood: %.1f\n\n",
            fitC$N, fitC$converged, fitC$logLik))

register_output("Model C — MCID EQ-5D", tabC, "table",
                sprintf("Logistic: mcid_eq5d_12m | N = %d", fitC$N))
register_output("Forest C — MCID EQ-5D",
                .forest_fn(tabC, "MCID EQ-5D at 12m — Odds Ratios"),
                "plot",
                "OR (95% CI). Dashed line = no effect (OR = 1).")

cat("Done. Outputs registered:", length(get_outputs()), "\n")
