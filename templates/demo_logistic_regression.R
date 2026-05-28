# templates/demo_logistic_regression.R
# =====================================================================
# TEMPLATE: Federated Logistic Regression
#
# Demonstrates fed_logistic_newton() for binary outcome regression
# across federated sites. Uses distributed Newton–IRLS: each site
# computes gradient/Hessian at the current beta; the coordinator
# updates beta and iterates to convergence. Results are exact.
#
# VARIABLES USED: mcid_arm_12m, mcid_eq5d_12m, mcid_ndi_12m,
#                 patient_satisfied_12m, age, sexM, bmi, smoker,
#                 nrs_arm_preop, eq5d_index_preop, ndi_index_preop,
#                 diag_myelo, asa, opioids_preop, comorbid_cardiac
#
# STANDALONE:  Rscript templates/demo_logistic_regression.R
# GUI:         Load this file in the coordinator app
# =====================================================================

library(fedstats)

ANALYSIS_TITLE <- "Federated Logistic Regression — MCID & Satisfaction"

VARS_SPEC <- list(
  mcid_arm_12m          = list(type = "binary"),
  mcid_eq5d_12m         = list(type = "binary"),
  mcid_ndi_12m          = list(type = "binary"),
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
# fed_logistic_newton(servers, formula, robust_cluster, verbose, ...)
#
# Arguments:
#   servers        — list of server objects
#   formula        — R formula with binary (0/1) outcome on LHS
#   robust_cluster — TRUE = use cluster-robust (sandwich) SE when
#                    >= 2 sites are available (recommended)
#   verbose        — TRUE to print convergence info per iteration
#   max_iter       — maximum Newton iterations (default 25)
#   tol            — convergence tolerance (default 1e-8)
#
# Returns list with:
#   $coefficients  — log-odds estimates
#   $se            — model-based SE
#   $se_robust     — cluster-robust SE (NULL if only 1 site or not requested)
#   $z             — z-scores (computed below: beta / se)
#   $p             — p-values
#   $N             — complete-case sample size
#   $logLik        — log-likelihood at convergence
#   $iterations    — number of Newton steps taken
#   $converged     — TRUE/FALSE
#   $clusters      — number of sites used for robust SE (or NULL)
# =====================================================================

# Helper: build standard output table from a fitted logistic model
.logit_table <- function(fit) {
  .se  <- if (!is.null(fit$se_robust)) fit$se_robust else fit$se
  .z   <- fit$coefficients / .se
  .p   <- 2 * (1 - pnorm(abs(.z)))
  tab  <- data.frame(
    Term      = names(fit$coefficients),
    log.OR    = round(fit$coefficients,      4),
    SE        = round(.se,                   4),
    z         = round(.z,                    3),
    p         = round(.p,                    4),
    OR        = round(exp(fit$coefficients), 4),
    CI.lower  = round(exp(fit$coefficients - 1.96 * .se), 4),
    CI.upper  = round(exp(fit$coefficients + 1.96 * .se), 4),
    stringsAsFactors = FALSE
  )
  rownames(tab) <- NULL
  tab
}

# Helper: make a forest-plot closure from a result table + title
.forest_fn <- function(tab, title) {
  local({
    .tab <- tab; .ttl <- title
    function() {
      .r  <- .tab[.tab$Term != "(Intercept)", ]
      .n  <- nrow(.r)
      if (.n == 0) { plot.new(); text(0.5, 0.5, "No predictors."); return() }
      .y  <- seq(.n, 1)
      .x_lo <- max(0.05, min(.r$CI.lower, na.rm = TRUE) * 0.75)
      .x_hi <- max(.r$CI.upper, na.rm = TRUE) * 1.4
      .old  <- par(mar = c(4.5, 10, 3.5, 2.5))
      on.exit(par(.old), add = TRUE)
      plot(.r$OR, .y,
           xlim = c(.x_lo, .x_hi), ylim = c(0.3, .n + 0.7),
           xlab = "Odds ratio (95% CI)", ylab = "",
           yaxt = "n", log = "x", pch = 15, cex = 1.4, col = "#2b6cb0",
           main = .ttl)
      for (.i in seq_len(.n))
        lines(c(.r$CI.lower[.i], .r$CI.upper[.i]),
              c(.y[.i], .y[.i]), col = "#2b6cb0", lwd = 2)
      abline(v = 1, lty = 2, col = "grey60")
      axis(2, at = .y, labels = .r$Term, las = 1, cex.axis = 0.85)
      for (.i in seq_len(.n))
        text(.r$OR[.i], .y[.i] + 0.38,
             sprintf("%.2f [%.2f–%.2f]",
                     .r$OR[.i], .r$CI.lower[.i], .r$CI.upper[.i]),
             cex = 0.72, col = "#2b6cb0")
    }
  })
}

# =====================================================================
# Model A: MCID arm NRS (≥ 3 point improvement)
# =====================================================================
cat("=== Model A: MCID arm NRS at 12m ===\n")

fitA <- fed_logistic_newton(
  servers,
  mcid_arm_12m ~ age + sexM + bmi + smoker +
    nrs_arm_preop + diag_myelo + asa + opioids_preop,
  robust_cluster = TRUE, verbose = FALSE
)

tabA <- .logit_table(fitA)
cat(sprintf("  N=%d | logLik=%.3f | converged=%s\n\n",
            fitA$N, fitA$logLik, fitA$converged))

register_output("Model A — MCID arm NRS", tabA, "table",
                sprintf("Logistic: mcid_arm_12m | N=%d, logLik=%.3f", fitA$N, fitA$logLik))
register_output("Forest A — MCID arm NRS",
                .forest_fn(tabA, "MCID arm NRS at 12m — Odds Ratios"),
                "plot",
                "Odds ratios (95% CI) for MCID arm NRS at 12 months")

# =====================================================================
# Model B: Patient satisfaction at 12 months
# =====================================================================
cat("=== Model B: Patient satisfaction at 12m ===\n")

fitB <- fed_logistic_newton(
  servers,
  patient_satisfied_12m ~ age + sexM + bmi + smoker +
    nrs_arm_preop + eq5d_index_preop + ndi_index_preop +
    diag_myelo + comorbid_cardiac + opioids_preop,
  robust_cluster = TRUE, verbose = FALSE
)

tabB <- .logit_table(fitB)
cat(sprintf("  N=%d | logLik=%.3f | converged=%s\n\n",
            fitB$N, fitB$logLik, fitB$converged))

register_output("Model B — Satisfaction", tabB, "table",
                sprintf("Logistic: patient_satisfied_12m | N=%d, logLik=%.3f", fitB$N, fitB$logLik))
register_output("Forest B — Satisfaction",
                .forest_fn(tabB, "Patient Satisfaction at 12m — Odds Ratios"),
                "plot",
                "Odds ratios (95% CI) for patient satisfaction at 12 months")

# =====================================================================
# Model C: MCID EQ-5D at 12 months
# =====================================================================
cat("=== Model C: MCID EQ-5D at 12m ===\n")

fitC <- fed_logistic_newton(
  servers,
  mcid_eq5d_12m ~ age + sexM + eq5d_index_preop +
    diag_myelo + smoker + asa + opioids_preop,
  robust_cluster = TRUE, verbose = FALSE
)

tabC <- .logit_table(fitC)
cat(sprintf("  N=%d | logLik=%.3f | converged=%s\n\n",
            fitC$N, fitC$logLik, fitC$converged))

register_output("Model C — MCID EQ-5D", tabC, "table",
                sprintf("Logistic: mcid_eq5d_12m | N=%d, logLik=%.3f", fitC$N, fitC$logLik))
register_output("Forest C — MCID EQ-5D",
                .forest_fn(tabC, "MCID EQ-5D at 12m — Odds Ratios"),
                "plot",
                "Odds ratios (95% CI) for MCID EQ-5D at 12 months")

cat("Done. Outputs registered:", length(get_outputs()), "\n")
