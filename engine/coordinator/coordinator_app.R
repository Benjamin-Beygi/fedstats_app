# coordinator_app.R
# =====================================================================
# Federated Statistics — Coordinator GUI (Shiny)
#
# Generalised: loads any .R analysis script that follows the contract:
#   ANALYSIS_TITLE  — shown in the window title
#   VARS_SPEC       — used for pre-analysis validation
#   register_output(name, value, type, caption) — drives dynamic tabs
#
# Start with the launcher (double-click):
#   start_coordinator_gui.command  (macOS)
#   start_coordinator_gui.bat      (Windows)
#   bash start_coordinator_gui.sh  (Linux)
# =====================================================================
suppressPackageStartupMessages({
  library(shiny)
  library(httr)
  library(jsonlite)
  library(fedstats)
})

# ---------------------------------------------------------------
# Script metadata extraction
#
# Evaluates ONLY the lines before the "if (!exists("servers"...)"
# block — so we get ANALYSIS_TITLE and VARS_SPEC without triggering
# any analysis code.
# ---------------------------------------------------------------
.extract_meta <- function(script_path) {
  lines  <- readLines(script_path, warn = FALSE)
  cutoff <- grep("if\\s*\\(!exists\\s*\\(", lines)[1]
  if (is.na(cutoff)) cutoff <- length(lines) + 1L
  code   <- paste(lines[seq_len(cutoff - 1L)], collapse = "\n")
  env    <- new.env(parent = globalenv())
  tryCatch(eval(parse(text = code), envir = env), error = function(e) NULL)
  list(
    title = if (exists("ANALYSIS_TITLE", envir = env, inherits = FALSE))
              env$ANALYSIS_TITLE
            else tools::file_path_sans_ext(basename(script_path)),
    vars_spec = if (exists("VARS_SPEC", envir = env, inherits = FALSE))
                  env$VARS_SPEC
                else NULL
  )
}

# ---------------------------------------------------------------
# Utilities
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

get_urls <- function(txt) {
  raw <- trimws(strsplit(txt, "\n")[[1]])
  raw[nzchar(raw)]
}

ping_one <- function(url, token, idx) {
  tryCatch({
    hdrs <- if (nzchar(token))
      httr::add_headers(Authorization = paste("Bearer", token)) else NULL
    r <- httr::GET(paste0(sub("/+$", "", url), "/health"), hdrs, httr::timeout(8))
    if (httr::status_code(r) == 200) {
      rows <- as.integer(unlist(httr::content(r, as = "parsed")$rows))
      sprintf("Site %d  [OK]   n = %d rows   %s", idx, rows, url)
    } else {
      sprintf("Site %d  [ERR]  HTTP %d   %s", idx, httr::status_code(r), url)
    }
  }, error = function(e) sprintf("Site %d  [ERR]  %s", idx, conditionMessage(e)))
}

# ---------------------------------------------------------------
# Coordinator's Tailscale IP for the URL placeholder
# ---------------------------------------------------------------
.ts_ip <- tryCatch({
  cmd <- if (.Platform$OS.type == "windows") "tailscale ip -4"
         else "tailscale ip -4 2>/dev/null"
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
    .step { font-weight: bold; font-size: 0.88em; margin: 14px 0 2px 0;
            color: #2b6cb0; }
    .meta-title { font-size: 1.0em; font-weight: bold; color: #2d3748;
                  margin: 4px 0 1px 0; }
    .meta-vars  { font-size: 0.78em; color: #555; margin-bottom: 4px;
                  word-break: break-word; }
    pre  { font-size: 0.80em; background: #f8f9fa; border: 1px solid #dee2e6;
           border-radius: 4px; padding: 10px; white-space: pre-wrap;
           max-height: 480px; overflow-y: auto; }
    .tbl-caption { font-size: 0.82em; color: #555; font-style: italic;
                   margin: 4px 0 14px 0; }
    .welcome { color: #718096; padding: 60px 20px; text-align: center; }
    .welcome h3 { color: #4a5568; }
    .note { font-size: 0.80em; color: #666; margin-top: 10px; }
  "))),

  uiOutput("app_title_ui"),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      # ---- Step 1: analysis script ----
      div(class = "step", "① Analysis script"),
      fileInput("script_file", NULL, accept = ".R",
                buttonLabel = "Browse…", placeholder = "No script selected"),
      uiOutput("script_meta_ui"),

      # ---- Step 2: site URLs ----
      div(class = "step", "② Site URLs  (one per line)"),
      textAreaInput("urls", NULL,
                    placeholder = .url_placeholder, rows = 5, width = "100%"),

      # ---- Step 3: optional token ----
      div(class = "step", "③ Security token"),
      passwordInput("token", NULL, placeholder = "(leave blank if none)"),

      br(),
      actionButton("btn_ping",     "Ping sites",
                   class = "btn-default btn-sm btn-block"),
      br(),
      actionButton("btn_validate", "Validate data",
                   class = "btn-primary btn-sm btn-block"),
      br(),
      actionButton("btn_run",      "Run analysis",
                   class = "btn-success btn-sm btn-block"),

      hr(),
      verbatimTextOutput("ping_out"),

      div(class = "note",
          "Only aggregate statistics leave each site.",
          br(), "No individual patient data is transferred.")
    ),

    mainPanel(
      width = 9,
      uiOutput("results_panel")
    )
  )
)

# ---------------------------------------------------------------
# Server
# ---------------------------------------------------------------
server <- function(input, output, session) {

  rv <- reactiveValues(
    meta     = NULL,   # list(title, vars_spec)
    outputs  = NULL,   # list of register_output() entries
    val_txt  = NULL    # text from standalone Validate
  )

  # ---- Dynamic app title from script ----------------------------
  output$app_title_ui <- renderUI({
    title <- if (!is.null(rv$meta)) rv$meta$title
             else "Federated Analysis — Coordinator"
    titlePanel(title)
  })

  # ---- Load script: extract metadata ----------------------------
  observeEvent(input$script_file, {
    req(input$script_file)
    rv$meta    <- tryCatch(
      .extract_meta(input$script_file$datapath),
      error = function(e) {
        showNotification(paste("Could not read script:", e$message),
                         type = "error"); NULL
      })
    rv$outputs <- NULL
    rv$val_txt <- NULL
  })

  output$script_meta_ui <- renderUI({
    m <- rv$meta
    if (is.null(m)) return(NULL)
    vs_names <- if (!is.null(m$vars_spec)) names(m$vars_spec) else character(0)
    tagList(
      div(class = "meta-title", m$title),
      if (length(vs_names) > 0)
        div(class = "meta-vars",
            strong("Variables: "), paste(vs_names, collapse = ", "))
    )
  })

  # ---- Ping -----------------------------------------------------
  observeEvent(input$btn_ping, {
    urls <- get_urls(input$urls)
    if (!length(urls)) {
      showNotification("Enter at least one site URL.", type = "warning")
      return()
    }
    tok <- trimws(input$token)
    withProgress(message = "Pinging sites…", {
      lines <- mapply(ping_one, urls, idx = seq_along(urls),
                      MoreArgs = list(token = tok), SIMPLIFY = TRUE)
    })
    output$ping_out <- renderText(paste(lines, collapse = "\n"))
  })

  # ---- Validate data --------------------------------------------
  observeEvent(input$btn_validate, {
    urls <- get_urls(input$urls)
    if (!length(urls)) {
      showNotification("Enter site URLs first.", type = "warning"); return()
    }
    if (is.null(rv$meta) || is.null(rv$meta$vars_spec)) {
      showNotification("Load an analysis script first.", type = "warning"); return()
    }
    tok  <- trimws(input$token)
    ss   <- lapply(urls, function(u) create_remote_server(u, tok))

    withProgress(message = "Validating sites…", {
      v <- tryCatch(
        fed_validate(ss, rv$meta$vars_spec, formula = NULL, min_n = 20L),
        error = function(e) list(ok = FALSE,
          errors = conditionMessage(e), warnings = character(0),
          site_reports = list(), heterogeneity = list())
      )
    })

    rv$val_txt  <- cap_print(print_validation_report(v))
    rv$outputs  <- NULL   # clear old results so validation tab shows

    if (v$ok)
      showNotification("Validation passed. Ready to run analysis.",
                       type = "message")
    else
      showNotification(
        sprintf("%d error(s) found — review the Validation tab.",
                length(v$errors)), type = "error", duration = 8)
  })

  # ---- Run analysis ---------------------------------------------
  observeEvent(input$btn_run, {
    urls <- get_urls(input$urls)
    if (!length(urls)) {
      showNotification("Enter site URLs first.", type = "warning"); return()
    }
    if (is.null(rv$meta)) {
      showNotification("Load an analysis script first.", type = "warning"); return()
    }
    tok    <- trimws(input$token)
    ss     <- lapply(urls, function(u) create_remote_server(u, tok))
    script <- input$script_file$datapath

    withProgress(
      message = paste("Running:", rv$meta$title), value = 0, {

      incProgress(0.05, detail = "Preparing…")
      clear_outputs()
      env         <- new.env(parent = globalenv())
      env$servers <- ss

      incProgress(0.10, detail = "Sourcing analysis script…")
      tryCatch(
        source(script, local = env),
        error = function(e) {
          showNotification(paste("Script error:", conditionMessage(e)),
                           type = "error", duration = 15)
        }
      )

      incProgress(1, detail = "Done")
    })

    result <- get_outputs()
    if (length(result) == 0) {
      showNotification(
        "No outputs were registered. Check the script calls register_output().",
        type = "warning")
    } else {
      rv$outputs <- result
      showNotification(
        sprintf("Done — %d output(s) ready.", length(result)),
        type = "message")
    }
  })

  # ---- Main panel -----------------------------------------------
  output$results_panel <- renderUI({

    # No script loaded: welcome screen
    if (is.null(rv$meta)) {
      return(div(class = "welcome",
        h3("Federated Analysis Coordinator"),
        br(),
        p("① Load an analysis script  (.R file that follows the contract)"),
        p("② Enter the site URLs"),
        p("③ Ping → Validate → Run Analysis")
      ))
    }

    # Build tab list: always include Status tab, add output tabs if available
    all_tabs <- list()

    # Status tab: shows validation output or a ready-prompt
    status_body <- if (!is.null(rv$val_txt)) {
      tagList(h5("Validation report"), verbatimTextOutput("val_display"))
    } else if (is.null(rv$outputs)) {
      div(class = "welcome",
          h4(rv$meta$title),
          p("Script loaded. Click Validate or Run Analysis."))
    } else {
      div(class = "welcome",
          p("Analysis complete. See output tabs above."))
    }
    all_tabs[[1]] <- tabPanel("Status", br(), status_body)

    # Dynamic output tabs
    if (!is.null(rv$outputs)) {
      for (i in seq_along(rv$outputs)) {
        out <- rv$outputs[[i]]
        id  <- paste0("dyn_", i)
        inner <- switch(out$type,
          "table" = tableOutput(id),
          "plot"  = plotOutput(id, height = "440px"),
          "text"  = verbatimTextOutput(id)
        )
        all_tabs[[length(all_tabs) + 1L]] <- tabPanel(
          out$name,
          br(),
          inner,
          if (!is.null(out$caption))
            div(class = "tbl-caption", out$caption)
        )
      }
    }

    do.call(tabsetPanel, c(list(id = "dyn_tabs"), all_tabs))
  })

  # val_display is always registered; shown inside the Status tab
  output$val_display <- renderText({
    if (is.null(rv$val_txt)) "" else rv$val_txt
  })

  # ---- Register a renderer for every dynamic output ---------------
  # Fires whenever rv$outputs changes (i.e. after Run Analysis).
  # Uses local() to capture loop variables correctly.
  observeEvent(rv$outputs, {
    outputs <- rv$outputs
    if (is.null(outputs)) return()

    for (i in seq_along(outputs)) {
      local({
        out <- outputs[[i]]
        id  <- paste0("dyn_", i)

        if (out$type == "table") {

          output[[id]] <- renderTable(
            out$value,
            striped = TRUE, hover = TRUE, na = ""
          )

        } else if (out$type == "plot") {

          fn <- out$value   # capture the closure
          output[[id]] <- renderPlot({
            if (is.function(fn))        fn()
            else if (inherits(fn, "ggplot")) print(fn)
            else { plot.new(); text(0.5, 0.5, "Cannot render plot.", cex = 1.2) }
          }, height = 440)

        } else {   # "text"

          txt <- out$value
          output[[id]] <- renderText(
            if (is.character(txt)) txt
            else cap_print(print(txt))
          )

        }
      })
    }

    # Switch to the first output tab automatically
    if (length(outputs) > 0)
      updateTabsetPanel(session, "dyn_tabs",
                        selected = outputs[[1]]$name)

  }, ignoreInit = TRUE)
}

shinyApp(ui, server)
