# swespine_analysis.R
# =====================================================================
# Swespine Cervical Registry — Federated Analysis
#
# SCRIPT CONTRACT  (required by the coordinator GUI)
# ------------------------------------------------------------------
# 1. ANALYSIS_TITLE  character scalar — shown in the GUI header
# 2. VARS_SPEC       named list — expected variables; the GUI runs
#                    fed_validate() against this before executing
# 3. servers         list of server objects — injected by the GUI at
#                    runtime; the standalone fallback below creates
#                    servers from CSV files in data/ for direct testing
# 4. register_output(name, value, type, caption)
#                    called for every result to be shown as a GUI tab
#
# STANDALONE TESTING (no GUI required):
#
#   With real remote sites (set FED_SITE_URLS env var):
#     FED_SITE_URLS="http://100.x.x.x:8001,http://100.x.x.x:8002" \
#       Rscript swespine_analysis.R
#
#   With local CSV files (one CSV = one simulated site):
#     Rscript swespine_analysis.R
#     (needs CSV files in ./data/; data/sweden.csv etc. work out of the box)
# =====================================================================

library(fedstats)

# ---- Analysis identity (shown in GUI header) ----
ANALYSIS_TITLE <- "Swespine Cervical Registry — Federated Analysis"

# ---- Expected variable specification ----
# The GUI reads this list to run pre-analysis validation before
# sourcing the rest of this script.
VARS_SPEC <- list(
  age           = list(type = "numeric",     min = 18,  max = 100),
  sexM          = list(type = "binary"),
  bmi           = list(type = "numeric",     min = 10,  max = 80),
  nrs_arm_preop = list(type = "numeric",     min = 0,   max = 10),
  nrs_arm_12m   = list(type = "numeric",     min = 0,   max = 10),
  mcid_arm_12m  = list(type = "binary"),
  diag_grp      = list(type = "categorical",
                        levels = c("radiculopathy", "myelopathy")),
  diag_myelo    = list(type = "binary")
)

# =====================================================================
# Standalone fallback — skipped when GUI injects `servers`.
# Priority: FED_SITE_URLS env var  >  ./data/*.csv files
# =====================================================================
if (!exists("servers", inherits = FALSE)) {

  .site_urls_env <- Sys.getenv("FED_SITE_URLS", unset = "")
  .token_env     <- Sys.getenv("FED_API_TOKEN", unset = "")

  if (nzchar(.site_urls_env)) {

    # ---- Remote sites via FED_SITE_URLS --------------------------------
    .urls <- trimws(strsplit(.site_urls_env, ",")[[1]])
    .urls <- .urls[nzchar(.urls)]
    if (length(.urls) == 0)
      stop("FED_SITE_URLS is set but contains no valid URLs.")

    cat("=====================================================\n")
    cat(" swespine_analysis.R — REMOTE MODE\n")
    cat("=====================================================\n")
    for (.u in .urls) cat(sprintf("  Site: %s\n", .u))
    cat("\n")

    servers <- lapply(.urls, function(u) {
      create_remote_server(u, token = if (nzchar(.token_env)) .token_env else NULL)
    })

  } else {

    # ---- Local CSV files -----------------------------------------------
    .csv_dir   <- file.path(getwd(), "data")
    .csv_files <- if (dir.exists(.csv_dir))
      sort(list.files(.csv_dir, pattern = "[.]csv$", full.names = TRUE)) else character(0)

    if (length(.csv_files) == 0)
      stop(
        "No sites configured.\n",
        "  Option A (remote):  FED_SITE_URLS=\"http://host:port,...\" Rscript swespine_analysis.R\n",
        "  Option B (local):   place CSV files in ./data/  (data/sweden.csv etc. are ready to use)"
      )

    cat("=====================================================\n")
    cat(" swespine_analysis.R — LOCAL CSV MODE\n")
    cat("=====================================================\n")
    servers <- lapply(.csv_files, function(f) {
      cat(sprintf("  Loading: %s\n", basename(f)))
      create_server(f)
    })
    cat(sprintf("  Sites loaded: %d\n\n", length(servers)))
  }
}

# =====================================================================
# Reset output registry before this run
# =====================================================================
clear_outputs()

# =====================================================================
# 1.  Pre-analysis validation
# =====================================================================
cat("----- Data validation -----\n")

validation <- fed_validate(
  servers,
  VARS_SPEC,
  formula = mcid_arm_12m ~ age + sexM,
  min_n   = 20L
)
print_validation_report(validation)

# Register a formatted text summary for the GUI
.val_lines <- c(
  sprintf("Sites checked : %d", length(validation$site_reports)),
  sprintf("Status        : %s", if (validation$ok) "PASS" else "FAIL"),
  ""
)
if (length(validation$errors) > 0)
  .val_lines <- c(.val_lines, "ERRORS:", paste0("  ", validation$errors), "")
if (length(validation$warnings) > 0)
  .val_lines <- c(.val_lines, "WARNINGS:", paste0("  ", validation$warnings), "")
if (length(validation$heterogeneity) > 0) {
  .val_lines <- c(.val_lines, sprintf("Heterogeneity (I²) across %d sites:",
                                      length(validation$site_reports)))
  for (.v in names(validation$heterogeneity)) {
    .h <- validation$heterogeneity[[.v]]
    .val_lines <- c(.val_lines,
                    sprintf("  %-20s I² = %5.1f%%  (pooled mean = %.3f)",
                            paste0(.v, ":"), .h$i2, .h$pooled_mean))
  }
}

register_output(
  name    = "Validation",
  value   = paste(.val_lines, collapse = "\n"),
  type    = "text",
  caption = "Pre-analysis data validation across all sites"
)

# =====================================================================
# 2.  Descriptive statistics
# =====================================================================
cat("\n----- Descriptive statistics -----\n")

.age_s    <- fed_numeric(servers, "age")
.bmi_s    <- fed_numeric(servers, "bmi")
.nrspre_s <- fed_numeric(servers, "nrs_arm_preop")
.nrs12_s  <- fed_numeric(servers, "nrs_arm_12m")
.mcid_s   <- fed_numeric(servers, "mcid_arm_12m")

cat(sprintf("Age:       n = %d | mean = %.2f | SD = %.2f\n",
            .age_s$n,    .age_s$mean,    .age_s$sd))
cat(sprintf("BMI:       n = %d | mean = %.2f | SD = %.2f\n",
            .bmi_s$n,    .bmi_s$mean,    .bmi_s$sd))
cat(sprintf("NRS preop: n = %d | mean = %.2f | SD = %.2f\n",
            .nrspre_s$n, .nrspre_s$mean, .nrspre_s$sd))
cat(sprintf("NRS 12m:   n = %d | mean = %.2f | SD = %.2f\n",
            .nrs12_s$n,  .nrs12_s$mean,  .nrs12_s$sd))
cat(sprintf("MCID:      %.0f / %d (%.1f%%)\n",
            .mcid_s$sum, .mcid_s$n, 100 * .mcid_s$mean))

desc_tab <- data.frame(
  Variable   = c("Age (years)", "BMI (kg/m²)",
                 "NRS arm — preop", "NRS arm — 12 months",
                 "MCID achieved (arm NRS)"),
  N          = c(.age_s$n, .bmi_s$n, .nrspre_s$n, .nrs12_s$n, .mcid_s$n),
  "Mean / %" = c(round(.age_s$mean,    2),
                 round(.bmi_s$mean,    2),
                 round(.nrspre_s$mean, 2),
                 round(.nrs12_s$mean,  2),
                 round(100 * .mcid_s$mean, 1)),
  SD         = c(round(.age_s$sd,    2),
                 round(.bmi_s$sd,    2),
                 round(.nrspre_s$sd, 2),
                 round(.nrs12_s$sd,  2),
                 NA_real_),
  check.names      = FALSE,
  stringsAsFactors = FALSE
)

register_output(
  name    = "Descriptives",
  value   = desc_tab,
  type    = "table",
  caption = "Descriptive statistics — pooled across all sites"
)

# =====================================================================
# 3.  Welch t-test: age by diagnosis group
# =====================================================================
cat("\n----- Welch t-test: age by diagnosis -----\n")

tt <- fed_welch_t(servers, "age", "diag_grp", "radiculopathy", "myelopathy")

cat(sprintf("  Radiculopathy: mean = %.2f (SD %.2f), n = %d\n",
            tt$mean1, tt$sd1, tt$n1))
cat(sprintf("  Myelopathy:    mean = %.2f (SD %.2f), n = %d\n",
            tt$mean2, tt$sd2, tt$n2))
cat(sprintf("  t = %.3f | df = %.1f | p = %.4g\n", tt$t, tt$df, tt$p))

ttest_tab <- data.frame(
  Group    = c("Radiculopathy", "Myelopathy"),
  N        = c(tt$n1, tt$n2),
  Mean_age = c(round(tt$mean1, 2), round(tt$mean2, 2)),
  SD_age   = c(round(tt$sd1,   2), round(tt$sd2,   2)),
  stringsAsFactors = FALSE
)

register_output(
  name    = "Age by diagnosis",
  value   = ttest_tab,
  type    = "table",
  caption = sprintf(
    "Welch t-test — age by diagnosis: t = %.3f, df = %.1f, p = %.4g",
    tt$t, tt$df, tt$p
  )
)

# =====================================================================
# 4.  Chi-square 2x2: sex vs diagnosis
# =====================================================================
cat("\n----- Chi-square: sex vs diagnosis -----\n")

chi <- fed_chisq_2x2(servers, "sexM", "diag_myelo", correct = TRUE)
print(chi$table)
cat(sprintf("  X² = %.3f | df = %d | p = %.4g | N = %d\n",
            chi$statistic, chi$df, chi$p, chi$N))

# Convert 2x2 matrix to a data.frame with a row-label column
.chi_mat <- chi$table
chi_df <- data.frame(
  " "                    = rownames(.chi_mat),
  as.data.frame(.chi_mat, stringsAsFactors = FALSE),
  check.names      = FALSE,
  stringsAsFactors = FALSE
)
rownames(chi_df) <- NULL

register_output(
  name    = "Sex vs diagnosis",
  value   = chi_df,
  type    = "table",
  caption = sprintf(
    "Chi-square (Yates correction) — sex × diagnosis: X² = %.3f, df = %d, p = %.4g, N = %d",
    chi$statistic, chi$df, chi$p, chi$N
  )
)

# =====================================================================
# 5.  Linear regression: nrs_arm_12m ~ age + sexM
# =====================================================================
cat("\n----- Linear regression: nrs_arm_12m ~ age + sexM -----\n")

linear <- fed_lm(servers, nrs_arm_12m ~ age + sexM)

lm_tab <- data.frame(
  Term      = names(linear$coefficients),
  Estimate  = round(linear$coefficients, 4),
  Std.Error = round(linear$se,           4),
  t.value   = round(linear$t,            3),
  p.value   = round(linear$p,            4),
  stringsAsFactors = FALSE
)
rownames(lm_tab) <- NULL

print(lm_tab)
cat(sprintf("N = %d | Residual SE = %.4f | df = %d\n",
            linear$N, linear$sigma, linear$df))

register_output(
  name    = "Linear regression",
  value   = lm_tab,
  type    = "table",
  caption = sprintf(
    "OLS: nrs_arm_12m ~ age + sexM — N = %d, Residual SE = %.4f, df = %d",
    linear$N, linear$sigma, linear$df
  )
)

# =====================================================================
# 6.  Logistic regression: mcid_arm_12m ~ age + sexM
# =====================================================================
cat("\n----- Logistic regression: mcid_arm_12m ~ age + sexM -----\n")

logit <- fed_logistic_newton(
  servers, mcid_arm_12m ~ age + sexM,
  robust_cluster = TRUE, verbose = FALSE
)

# Use cluster-robust SEs when available (>= 2 sites)
.se_use <- if (!is.null(logit$se_robust)) logit$se_robust else logit$se

.beta  <- logit$coefficients
.z     <- .beta / .se_use
.p_val <- 2 * (1 - pnorm(abs(.z)))

logit_tab <- data.frame(
  Term      = names(.beta),
  Estimate  = round(.beta,                   4),
  Std.Error = round(.se_use,                 4),
  z.value   = round(.z,                      3),
  p.value   = round(.p_val,                  4),
  OR        = round(exp(.beta),              4),
  CI.lower  = round(exp(.beta - 1.96 * .se_use), 4),
  CI.upper  = round(exp(.beta + 1.96 * .se_use), 4),
  stringsAsFactors = FALSE
)
rownames(logit_tab) <- NULL

print(logit_tab)
cat(sprintf("N = %d | logLik = %.3f | iterations = %d | converged = %s%s\n",
            logit$N, logit$logLik, logit$iterations, logit$converged,
            if (!is.null(logit$clusters))
              sprintf(" | cluster-robust SE (%d clusters)", logit$clusters)
            else ""))

register_output(
  name    = "Logistic regression",
  value   = logit_tab,
  type    = "table",
  caption = sprintf(
    "Logistic: mcid_arm_12m ~ age + sexM — N = %d, logLik = %.3f%s",
    logit$N, logit$logLik,
    if (!is.null(logit$clusters))
      sprintf(", cluster-robust SE (%d sites)", logit$clusters)
    else ""
  )
)

# =====================================================================
# 7.  Forest plot (OR plot for logistic regression)
# =====================================================================
# Registered as a zero-argument closure so the GUI calls it lazily
# inside renderPlot() without re-running the whole analysis.

register_output(
  name  = "Forest plot",
  value = local({

    # Capture what we need — survives the function call boundary
    .lt  <- logit_tab

    function() {
      .rows <- .lt[.lt$Term != "(Intercept)", ]
      if (nrow(.rows) == 0) {
        plot.new()
        text(0.5, 0.5, "No predictors to plot.", cex = 1.2)
        return(invisible(NULL))
      }

      .n     <- nrow(.rows)
      .y_pos <- seq(.n, 1)         # top-to-bottom order

      # X-axis limits on log scale (clip extreme CIs)
      .x_lo <- max(0.05, min(.rows$CI.lower, na.rm = TRUE) * 0.75)
      .x_hi <- max(.rows$CI.upper, na.rm = TRUE) * 1.35

      .old_par <- par(mar = c(4.5, 8, 3.5, 2.5))
      on.exit(par(.old_par), add = TRUE)

      plot(
        .rows$OR, .y_pos,
        xlim = c(.x_lo, .x_hi), ylim = c(0.3, .n + 0.7),
        xlab = "Odds ratio (95% CI)", ylab = "",
        yaxt = "n", log = "x",
        pch  = 15, cex = 1.5, col = "steelblue4",
        main = "Logistic regression: mcid_arm_12m ~ age + sexM"
      )

      # 95% CI whiskers
      for (.i in seq_len(.n))
        lines(
          c(.rows$CI.lower[.i], .rows$CI.upper[.i]),
          c(.y_pos[.i], .y_pos[.i]),
          col = "steelblue4", lwd = 2
        )

      # Reference line at OR = 1
      abline(v = 1, lty = 2, col = "grey55")

      # Y-axis term labels
      axis(2, at = .y_pos, labels = .rows$Term, las = 1, cex.axis = 0.95)

      # Numeric annotations above each point
      for (.i in seq_len(.n))
        text(
          .rows$OR[.i], .y_pos[.i] + 0.38,
          labels = sprintf("%.2f [%.2f–%.2f]",
                           .rows$OR[.i], .rows$CI.lower[.i], .rows$CI.upper[.i]),
          cex = 0.78, col = "steelblue4"
        )
    }
  }),
  type    = "plot",
  caption = "Odds ratios with 95% CI — logistic regression (mcid_arm_12m ~ age + sexM)"
)

# =====================================================================
# Done
# =====================================================================
cat("\n=====================================================\n")
cat(" Analysis complete.\n")
cat(sprintf(" Outputs registered: %d\n", length(get_outputs())))
cat("=====================================================\n")
