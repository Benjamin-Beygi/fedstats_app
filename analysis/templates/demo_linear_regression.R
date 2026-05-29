# templates/demo_linear_regression.R
# =====================================================================
# TEMPLATE: Federated Linear Regression (OLS)
#
# Demonstrates fed_lm() for ordinary least-squares regression across
# federated sites. Uses the distributed sufficient-statistics method:
# each site returns (X'X, X'y, y'y), and the coordinator solves the
# pooled normal equations. The result is numerically identical to
# running lm() on the combined dataset.
#
# VARIABLES USED: delta_nrs_arm_12m, delta_eq5d_12m, delta_ndi_12m,
#                 age, sexM, bmi, smoker, nrs_arm_preop, eq5d_index_preop,
#                 ndi_index_preop, diag_myelo, asa, opioids_preop,
#                 comorbid_cardiac
#
# STANDALONE:  Rscript templates/demo_linear_regression.R
# GUI:         Load this file in the coordinator app
# =====================================================================

library(fedstats)

ANALYSIS_TITLE <- "Federated OLS — Linear Regression Templates"

VARS_SPEC <- list(
  delta_nrs_arm_12m  = list(type = "numeric", min = -10, max = 10),
  delta_eq5d_12m     = list(type = "numeric", min = -2,  max = 2),
  delta_ndi_12m      = list(type = "numeric", min = -100, max = 100),
  age                = list(type = "numeric", min = 18, max = 100),
  sexM               = list(type = "binary"),
  bmi                = list(type = "numeric", min = 10, max = 80),
  smoker             = list(type = "binary"),
  nrs_arm_preop      = list(type = "numeric", min = 0,  max = 10),
  eq5d_index_preop   = list(type = "numeric", min = -1, max = 1),
  ndi_index_preop    = list(type = "numeric", min = 0,  max = 100),
  diag_myelo         = list(type = "binary"),
  asa                = list(type = "numeric", min = 1,  max = 5),
  opioids_preop      = list(type = "binary"),
  comorbid_cardiac   = list(type = "binary")
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
# fed_lm(servers, formula)
#
# Arguments:
#   servers  — list of server objects
#   formula  — standard R formula, e.g.  outcome ~ x1 + x2 + x3
#              Use standard R formula coding:
#                - Categorical vars must be pre-coded as binary (0/1)
#                - Interaction terms: x1:x2 or x1*x2
#
# Returns list with:
#   $coefficients — named vector of OLS estimates
#   $se           — standard errors
#   $t            — t-statistics
#   $p            — p-values
#   $N            — number of complete-case observations
#   $df           — residual degrees of freedom
#   $sigma        — residual standard error
# =====================================================================

# =====================================================================
# Model 1: NRS arm improvement ~ demographics + pre-op scores + diagnosis
# =====================================================================
cat("=== Model 1: NRS arm change at 12m ===\n")

m1 <- fed_lm(
  servers,
  delta_nrs_arm_12m ~ age + sexM + bmi + smoker +
    nrs_arm_preop + diag_myelo + asa + opioids_preop
)

.make_lm_tab <- function(fit) {
  tab <- data.frame(
    Term      = names(fit$coefficients),
    Estimate  = round(fit$coefficients, 4),
    Std.Error = round(fit$se,           4),
    t.value   = round(fit$t,            3),
    p.value   = round(fit$p,            4),
    stringsAsFactors = FALSE
  )
  rownames(tab) <- NULL
  tab
}

m1_tab <- .make_lm_tab(m1)
print(m1_tab)
cat(sprintf("N = %d | Residual SE = %.4f | df = %d\n\n",
            m1$N, m1$sigma, m1$df))

register_output(
  "Model 1 — NRS arm change",
  m1_tab,
  "table",
  sprintf("OLS: delta_nrs_arm_12m ~ demographics + pre-op + diagnosis | N=%d, σ=%.4f",
          m1$N, m1$sigma)
)

# =====================================================================
# Model 2: EQ-5D change at 12m
# =====================================================================
cat("=== Model 2: EQ-5D change at 12m ===\n")

m2 <- fed_lm(
  servers,
  delta_eq5d_12m ~ age + sexM + bmi + smoker +
    eq5d_index_preop + diag_myelo + asa + comorbid_cardiac
)

m2_tab <- .make_lm_tab(m2)
print(m2_tab)
cat(sprintf("N = %d | Residual SE = %.4f | df = %d\n\n",
            m2$N, m2$sigma, m2$df))

register_output(
  "Model 2 — EQ-5D change",
  m2_tab,
  "table",
  sprintf("OLS: delta_eq5d_12m ~ demographics + pre-op + diagnosis | N=%d, σ=%.4f",
          m2$N, m2$sigma)
)

# =====================================================================
# Model 3: NDI change at 12m
# =====================================================================
cat("=== Model 3: NDI change at 12m ===\n")

m3 <- fed_lm(
  servers,
  delta_ndi_12m ~ age + sexM + bmi + smoker +
    ndi_index_preop + diag_myelo + asa + opioids_preop
)

m3_tab <- .make_lm_tab(m3)
print(m3_tab)
cat(sprintf("N = %d | Residual SE = %.4f | df = %d\n\n",
            m3$N, m3$sigma, m3$df))

register_output(
  "Model 3 — NDI change",
  m3_tab,
  "table",
  sprintf("OLS: delta_ndi_12m ~ demographics + pre-op + diagnosis | N=%d, σ=%.4f",
          m3$N, m3$sigma)
)

# =====================================================================
# Coefficient comparison plot across all three models
# =====================================================================
register_output(
  "Coefficient plot (3 models)",
  value = local({
    .m1 <- m1_tab; .m2 <- m2_tab; .m3 <- m3_tab
    function() {
      # common terms (excluding intercept)
      .common <- intersect(
        intersect(.m1$Term[.m1$Term != "(Intercept)"],
                  .m2$Term[.m2$Term != "(Intercept)"]),
        .m3$Term[.m3$Term != "(Intercept)"]
      )
      if (!length(.common)) { plot.new(); text(0.5, 0.5, "No common terms."); return() }
      .n <- length(.common)
      .y  <- seq(.n, 1)
      par(mar = c(4.5, 9, 3.5, 1.5))
      .xlim <- c(min(c(.m1$Estimate, .m2$Estimate, .m3$Estimate)) * 1.3,
                 max(c(.m1$Estimate, .m2$Estimate, .m3$Estimate)) * 1.3)
      plot(NULL, xlim = .xlim, ylim = c(0.3, .n + 1.4),
           xlab = "Coefficient estimate", ylab = "",
           yaxt = "n", main = "Regression coefficients — 3 outcome models")
      abline(v = 0, lty = 2, col = "grey65")
      .cols <- c("steelblue4", "#276749", "#c05621")
      .labels <- c("NRS arm Δ", "EQ-5D Δ", "NDI Δ")
      for (.mi in 1:3) {
        .tab <- list(.m1, .m2, .m3)[[.mi]]
        .r <- .tab[.tab$Term %in% .common, ]
        .r <- .r[match(.common, .r$Term), ]
        .off <- (.mi - 2) * 0.18
        points(.r$Estimate, .y + .off, pch = 15 + .mi - 1,
               cex = 1.2, col = .cols[.mi])
        for (.j in seq_len(.n))
          lines(c(.r$Estimate[.j] - 1.96 * .r$Std.Error[.j],
                  .r$Estimate[.j] + 1.96 * .r$Std.Error[.j]),
                c(.y[.j] + .off, .y[.j] + .off),
                col = .cols[.mi], lwd = 1.5)
      }
      axis(2, at = .y, labels = .common, las = 1, cex.axis = 0.85)
      legend("topright", legend = .labels, col = .cols,
             pch = 15:17, cex = 0.82, bty = "n")
    }
  }),
  type    = "plot",
  caption = "OLS coefficient estimates (±95% CI) for common predictors across three outcome models"
)

cat("Done. Outputs registered:", length(get_outputs()), "\n")
