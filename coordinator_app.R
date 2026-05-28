# coordinator_app.R
# =====================================================================
# Federated Statistics — Coordinator GUI (Shiny)
#
# Start with the launcher (double-click):
#   start_coordinator_gui.command  (macOS)
#   start_coordinator_gui.bat      (Windows)
#   bash start_coordinator_gui.sh  (Linux)
#
# Or manually:
#   Rscript -e "shiny::runApp('coordinator_app.R', launch.browser=TRUE)"
# =====================================================================
suppressPackageStartupMessages({
  library(shiny)
  library(httr)
  library(jsonlite)
  library(fedstats)
})

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

# ---------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------
fmt_p <- function(p) {
  p <- suppressWarnings(as.numeric(p))
  if (is.na(p) || !is.finite(p)) return("")
  if (p < 0.001) "< 0.001" else sprintf("%.3f", p)
}

cap_print <- function(expr) {
  con <- textConnection("buf", "w", local = TRUE)
  sink(con); on.exit({ sink(); close(con) }, add = TRUE)
  force(expr)
  paste(buf, collapse = "\n")
}

get_urls <- function(text) {
  raw <- trimws(strsplit(text, "\n")[[1]])
  raw[nzchar(raw)]
}

ping_one <- function(url, token, idx) {
  tryCatch({
    hdrs <- if (nzchar(token))
      httr::add_headers(Authorization = paste("Bearer", token)) else NULL
    r <- httr::GET(paste0(sub("/+$", "", url), "/health"), hdrs, httr::timeout(8))
    if (httr::status_code(r) == 200) {
      rows <- as.integer(unlist(httr::content(r, as = "parsed")$rows))
      sprintf("Site %d  [OK]    n = %d rows   %s", idx, rows, url)
    } else {
      sprintf("Site %d  [ERR]   HTTP %d   %s", idx, httr::status_code(r), url)
    }
  }, error = function(e) {
    sprintf("Site %d  [ERR]   %s", idx, conditionMessage(e))
  })
}

# ---------------------------------------------------------------
# Detect coordinator's own Tailscale IP for the URL placeholder
# ---------------------------------------------------------------
.ts_ip <- tryCatch({
  cmd <- if (.Platform$OS.type == "windows") "tailscale ip -4" else "tailscale ip -4 2>/dev/null"
  ip  <- trimws(system(cmd, intern = TRUE, ignore.stderr = TRUE))
  if (length(ip) > 0 && nzchar(ip[1])) ip[1] else ""
}, error = function(e) "")

.url_placeholder <- if (nzchar(.ts_ip)) {
  sprintf("http://100.x.x.x:8000\nhttp://100.x.x.x:8001\n\n(Your Tailscale IP: %s)", .ts_ip)
} else {
  "http://100.x.x.x:8000\nhttp://100.x.x.x:8001"
}

# ---------------------------------------------------------------
# UI
# ---------------------------------------------------------------
ui <- fluidPage(
  tags$head(tags$style(HTML("
    body  { font-family: 'Helvetica Neue', Arial, sans-serif; }
    h5    { color: #2d3748; margin-top: 18px; }
    .step { font-weight: bold; font-size: 0.88em; margin: 10px 0 2px 0; color: #2b6cb0; }
    pre   { font-size: 0.82em; background: #f8f9fa; border: 1px solid #dee2e6;
            border-radius: 4px; padding: 10px; white-space: pre-wrap; }
    .note { font-size: 0.82em; color: #666; margin-top: 4px; }
    .tbl-caption { font-size: 0.82em; color: #555; font-style: italic;
                   margin: 2px 0 12px 0; }
  "))),

  titlePanel("Federated Spine Registry — Coordinator"),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      div(class = "step", "① Site URLs"),
      textAreaInput("urls", NULL,
        placeholder = .url_placeholder,
        rows = 5, width = "100%"),

      div(class = "step", "② Security token"),
      passwordInput("token", NULL, placeholder = "(leave blank if none)"),

      br(),
      actionButton("btn_ping",     "Ping sites",    class = "btn-default btn-sm btn-block"),
      br(),
      actionButton("btn_validate", "Validate data", class = "btn-primary btn-sm btn-block"),
      br(),
      actionButton("btn_run",      "Run analysis",  class = "btn-success btn-sm btn-block"),

      hr(),
      verbatimTextOutput("ping_out"),

      div(class = "note",
          "Only aggregate statistics leave each site.",
          br(), "No individual patient data is transferred.")
    ),

    mainPanel(
      width = 9,
      tabsetPanel(id = "tabs",

        tabPanel("Validation",
          br(),
          verbatimTextOutput("val_out")
        ),

        tabPanel("Descriptives",
          br(),
          h5("Summary statistics"),
          tableOutput("tbl_desc"),
          br(),
          h5("Age by diagnosis group (Welch t-test)"),
          tableOutput("tbl_ttest"),
          br(),
          h5("Sex × diagnosis (chi-square test)"),
          tableOutput("tbl_chisq"),
          verbatimTextOutput("txt_chisq_info")
        ),

        tabPanel("Linear regression",
          br(),
          p(strong("Outcome: "), "NRS arm pain at 12 months",
            br(), strong("Predictors: "), "age (years), sex (male = 1)"),
          tableOutput("tbl_lm"),
          div(class = "tbl-caption", verbatimTextOutput("txt_lm_info"))
        ),

        tabPanel("Logistic regression",
          br(),
          p(strong("Outcome: "), "MCID achieved (≥3-point NRS improvement at 12 months)",
            br(), strong("Predictors: "), "age (years), sex (male = 1)",
            br(), em("SE = cluster-robust (one cluster per site).")),
          tableOutput("tbl_logit"),
          div(class = "tbl-caption", verbatimTextOutput("txt_logit_info")),
          br(),
          plotOutput("forest", height = "200px")
        )
      )
    )
  )
)

# ---------------------------------------------------------------
# Server
# ---------------------------------------------------------------
server <- function(input, output, session) {

  active_servers <- reactiveVal(NULL)

  # ---- Ping -------------------------------------------------------
  observeEvent(input$btn_ping, {
    urls <- get_urls(input$urls)
    if (!length(urls)) {
      showNotification("Enter at least one site URL.", type = "warning")
      return()
    }
    tok <- input$token
    ss  <- lapply(urls, function(u) create_remote_server(u, tok))
    active_servers(ss)
    lines <- mapply(ping_one, urls, idx = seq_along(urls),
                    MoreArgs = list(token = tok), SIMPLIFY = TRUE)
    output$ping_out <- renderText(paste(lines, collapse = "\n"))
  })

  # ---- Validate ---------------------------------------------------
  observeEvent(input$btn_validate, {
    urls <- get_urls(input$urls)
    if (!length(urls)) {
      showNotification("Enter site URLs first.", type = "warning")
      return()
    }
    tok <- input$token
    ss  <- lapply(urls, function(u) create_remote_server(u, tok))
    active_servers(ss)

    withProgress(message = "Validating sites...", {
      v <- tryCatch(
        fed_validate(ss, VARS_SPEC, formula = mcid_arm_12m ~ age + sexM, min_n = 20L),
        error = function(e) list(ok = FALSE, errors = conditionMessage(e),
                                 warnings = character(0), site_reports = list(),
                                 heterogeneity = list())
      )
    })

    output$val_out <- renderText(cap_print(print_validation_report(v)))
    updateTabsetPanel(session, "tabs", "Validation")

    if (v$ok) showNotification("Validation passed. Ready to run analysis.", type = "message")
    else      showNotification(
                sprintf("%d error(s) found — review the Validation tab.",
                        length(v$errors)), type = "error", duration = 8)
  })

  # ---- Run analysis -----------------------------------------------
  observeEvent(input$btn_run, {
    urls <- get_urls(input$urls)
    if (!length(urls)) {
      showNotification("Enter site URLs first.", type = "warning")
      return()
    }
    tok <- input$token
    ss  <- lapply(urls, function(u) create_remote_server(u, tok))
    active_servers(ss)

    withProgress(message = "Running federated analysis...", value = 0, {

      # Validation (always runs first; blocks on error)
      incProgress(0.05, detail = "Validating")
      v <- tryCatch(
        fed_validate(ss, VARS_SPEC, formula = mcid_arm_12m ~ age + sexM, min_n = 20L),
        error = function(e) list(ok = FALSE, errors = conditionMessage(e),
                                 warnings = character(0), site_reports = list(),
                                 heterogeneity = list())
      )
      output$val_out <- renderText(cap_print(print_validation_report(v)))
      if (!v$ok) {
        showNotification("Validation failed — check the Validation tab.",
                         type = "error", duration = 10)
        updateTabsetPanel(session, "tabs", "Validation")
        return()
      }

      # ---- Descriptive statistics ---------------------------------
      incProgress(0.15, detail = "Descriptive statistics")

      age_s  <- fed_numeric(ss, "age")
      bmi_s  <- fed_numeric(ss, "bmi")
      nrsp_s <- fed_numeric(ss, "nrs_arm_preop")
      nrs12  <- fed_numeric(ss, "nrs_arm_12m")
      mcid_s <- fed_numeric(ss, "mcid_arm_12m")

      output$tbl_desc <- renderTable({
        data.frame(
          Variable = c("Age (years)", "BMI (kg/m²)",
                       "NRS arm pre-op (0–10)",
                       "NRS arm 12 months (0–10)",
                       "MCID arm 12 months"),
          N    = c(age_s$n, bmi_s$n, nrsp_s$n, nrs12$n, mcid_s$n),
          Mean = c(sprintf("%.1f", age_s$mean),  sprintf("%.1f", bmi_s$mean),
                   sprintf("%.2f", nrsp_s$mean), sprintf("%.2f", nrs12$mean),
                   sprintf("%.1f%%", 100 * mcid_s$mean)),
          SD   = c(sprintf("%.1f", age_s$sd),  sprintf("%.1f", bmi_s$sd),
                   sprintf("%.2f", nrsp_s$sd), sprintf("%.2f", nrs12$sd), ""),
          stringsAsFactors = FALSE
        )
      }, striped = TRUE, hover = TRUE)

      # ---- Welch t-test -------------------------------------------
      incProgress(0.25, detail = "Group comparisons")

      tt <- tryCatch(
        fed_welch_t(ss, "age", "diag_grp", "radiculopathy", "myelopathy"),
        error = function(e) NULL
      )
      if (!is.null(tt)) {
        output$tbl_ttest <- renderTable({
          data.frame(
            Group    = c("Radiculopathy", "Myelopathy"),
            N        = c(tt$n1, tt$n2),
            `Mean age` = sprintf("%.2f", c(tt$mean1, tt$mean2)),
            SD         = sprintf("%.2f", c(tt$sd1, tt$sd2)),
            `t (df)`   = c(sprintf("%.3f (%.1f)", tt$t, tt$df), ""),
            p          = c(fmt_p(tt$p), ""),
            check.names = FALSE, stringsAsFactors = FALSE
          )
        }, striped = TRUE)
      }

      # ---- Chi-square ---------------------------------------------
      chi <- tryCatch(
        fed_chisq_2x2(ss, "sexM", "diag_myelo"),
        error = function(e) NULL
      )
      if (!is.null(chi)) {
        output$tbl_chisq <- renderTable({
          tbl <- chi$table
          df_out <- as.data.frame.matrix(tbl)
          colnames(df_out) <- c("Radiculopathy", "Myelopathy")
          cbind(Sex = c("Female (sexM=0)", "Male (sexM=1)"), df_out)
        }, striped = TRUE, rownames = FALSE)
        output$txt_chisq_info <- renderText(
          sprintf("X² = %.3f   df = %d   p = %s   N = %d",
                  chi$statistic, chi$df, fmt_p(chi$p), chi$N)
        )
      }

      # ---- Linear regression --------------------------------------
      incProgress(0.45, detail = "Linear regression")

      lm_fit <- tryCatch(
        fed_lm(ss, nrs_arm_12m ~ age + sexM),
        error = function(e) NULL
      )
      if (!is.null(lm_fit)) {
        output$tbl_lm <- renderTable({
          data.frame(
            Term           = names(lm_fit$coefficients),
            Estimate       = round(lm_fit$coefficients, 4),
            `Std. Error`   = round(lm_fit$se, 4),
            `t value`      = round(lm_fit$t, 3),
            `p value`      = sapply(lm_fit$p, fmt_p),
            check.names = FALSE, stringsAsFactors = FALSE
          )
        }, striped = TRUE)
        output$txt_lm_info <- renderText(
          sprintf("N = %d   Residual SE = %.4f   df = %d",
                  lm_fit$N, lm_fit$sigma, lm_fit$df)
        )
      }

      # ---- Logistic regression ------------------------------------
      incProgress(0.70, detail = "Logistic regression")

      logit <- tryCatch(
        fed_logistic_newton(ss, mcid_arm_12m ~ age + sexM,
                            robust_cluster = TRUE, verbose = FALSE),
        error = function(e) NULL
      )
      if (!is.null(logit)) {
        se_use <- if (!is.null(logit$se_robust)) logit$se_robust else logit$se
        beta   <- logit$coefficients
        z_vals <- beta / se_use
        p_vals <- 2 * (1 - pnorm(abs(z_vals)))
        lo     <- exp(beta - 1.96 * se_use)
        hi     <- exp(beta + 1.96 * se_use)

        output$tbl_logit <- renderTable({
          data.frame(
            Term          = names(beta),
            `log OR`      = round(beta, 4),
            `Std. Error`  = round(se_use, 4),
            `z value`     = round(z_vals, 3),
            `p value`     = sapply(p_vals, fmt_p),
            OR            = round(exp(beta), 3),
            `95% CI`      = sprintf("[%.3f, %.3f]", lo, hi),
            check.names = FALSE, stringsAsFactors = FALSE
          )
        }, striped = TRUE)

        se_label <- if (!is.null(logit$se_robust))
          sprintf("cluster-robust SE (k = %d sites)", logit$clusters)
        else "standard SE"
        output$txt_logit_info <- renderText(
          sprintf("N = %d   log-lik = %.3f   iterations = %d   converged = %s   %s",
                  logit$N, logit$logLik, logit$iterations, logit$converged, se_label)
        )

        # Forest plot — non-intercept terms only
        noint <- names(beta)[names(beta) != "(Intercept)"]
        if (length(noint) > 0) {
          b_ni  <- beta[noint]
          se_ni <- se_use[noint]
          or_ni <- exp(b_ni)
          lo_ni <- exp(b_ni - 1.96 * se_ni)
          hi_ni <- exp(b_ni + 1.96 * se_ni)
          k     <- length(noint)

          output$forest <- renderPlot({
            label_x <- max(hi_ni) * 1.6   # right side for OR annotations
            xlim    <- c(min(lo_ni) * 0.7, label_x * 1.05)
            par(mar = c(4, 7, 3, 1))
            plot(or_ni, seq_len(k),
                 xlim = xlim, ylim = c(0.5, k + 0.5),
                 xlab = "Odds Ratio (95% CI)", ylab = "",
                 yaxt = "n", pch = 15, cex = 1.6, col = "#2b6cb0",
                 main = "Forest plot — MCID achieved at 12 months")
            abline(v = 1, lty = 2, col = "grey60")
            for (j in seq_len(k)) {
              lines(c(lo_ni[j], hi_ni[j]), c(j, j), lwd = 2.5, col = "#2b6cb0")
              text(label_x, j,
                   labels = sprintf("OR %.3f  [%.3f–%.3f]  p%s",
                                    or_ni[j], lo_ni[j], hi_ni[j],
                                    fmt_p(p_vals[noint[j]])),
                   adj = 1, cex = 0.82, col = "#333")
            }
            axis(2, at = seq_len(k), labels = noint, las = 1)
          })
        }
      }

      incProgress(1, detail = "Done")
    })

    updateTabsetPanel(session, "tabs", "Descriptives")
    showNotification("Analysis complete.", type = "message")
  })
}

shinyApp(ui, server)
