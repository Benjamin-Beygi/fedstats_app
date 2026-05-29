# engine/site/site_app.R
# =====================================================================
# Federated Statistics — Site Server GUI
#
# Launched by the Start Site launchers in Run/Mac, Run/Linux, Run/Windows.
# Runs the plumber API (api_server.R) as a background subprocess and
# streams its output into the browser log panel.
# =====================================================================
suppressPackageStartupMessages({
  library(shiny)
  library(processx)
})

# shiny::runApp() sets cwd to the app directory (engine/site/).
# Navigate up two levels to reach the project root.
.app_root    <- normalizePath(file.path(getwd(), "..", ".."))
.api_script  <- file.path(.app_root, "engine", "site", "api_server.R")
.fedstats_ok <- requireNamespace("fedstats", quietly = TRUE) && file.exists(.api_script)

# ---- Auto-detect CSV files ------------------------------------------
.scan_csvs <- function() {
  data_dir <- file.path(.app_root, "data")
  if (dir.exists(data_dir)) {
    found <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE)
    if (length(found)) return(found)
  }
  list.files(.app_root, pattern = "\\.csv$", full.names = TRUE, recursive = FALSE)
}
.csv_files <- .scan_csvs()

# ---- Tailscale IP ---------------------------------------------------
.ts_ip <- tryCatch({
  cmd <- if (.Platform$OS.type == "windows") "tailscale ip -4"
         else "tailscale ip -4 2>/dev/null"
  ip  <- trimws(system(cmd, intern = TRUE, ignore.stderr = TRUE))
  if (length(ip) && nzchar(ip[1])) ip[1] else ""
}, error = function(e) "")

# -----------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------
ui <- fluidPage(
  tags$head(tags$style(HTML("
    /* ── Karolinska colour palette ─────────────────────────────── */
    body { font-family:'Helvetica Neue',Arial,sans-serif;
           max-width:860px; margin:0 auto; padding:24px; }
    h3   { margin-bottom:0; color:#6A0DAD; }
    .bdg { display:inline-block; padding:3px 12px; border-radius:10px;
           font-size:.82em; font-weight:bold; margin-left:10px; vertical-align:middle; }
    .bdg-stopped  { background:#fed7d7; color:#9b2c2c; }
    .bdg-starting { background:#fefcbf; color:#744210; }
    .bdg-running  { background:#e9d5ff; color:#4a0080; }
    .addr { background:#f3e8ff; border:1px solid #c084fc; border-radius:6px;
            padding:10px 16px; margin:12px 0; }
    .addr-lbl { font-size:.75em; font-weight:bold; color:#6A0DAD;
                text-transform:uppercase; letter-spacing:.05em; }
    .addr-url { font-family:monospace; font-size:1.18em; color:#4a0080;
                font-weight:bold; margin-top:3px; }
    .privacy  { background:#f0fff4; border-left:3px solid #38a169;
                padding:8px 12px; margin:12px 0; font-size:.83em; color:#276749; }
    .warn-box { background:#fff5f5; border-left:3px solid #fc8181;
                padding:8px 12px; margin:12px 0; font-size:.83em; color:#c53030; }
    .sec-lbl  { font-weight:bold; font-size:.8em; color:#6A0DAD;
                text-transform:uppercase; letter-spacing:.05em; margin:18px 0 6px 0; }
    #server_log { height:280px; overflow-y:auto; background:#1a202c; color:#e2e8f0;
                  font-family:monospace; font-size:.76em; border-radius:6px;
                  padding:10px 14px; white-space:pre-wrap; border:none; }
    hr { border-color:#e2e8f0; margin:16px 0; }
    .btn-primary,
    .btn-primary:active,
    .btn-primary.active { background-color: #6A0DAD !important;
                          border-color: #5a0a91 !important; }
    .btn-primary:hover,
    .btn-primary:focus  { background-color: #5a0a91 !important;
                          border-color: #4a0080 !important; }
  "))),

  tags$script(HTML("
    Shiny.addCustomMessageHandler('scrollLog', function(x) {
      setTimeout(function() {
        var el = document.getElementById('server_log');
        if (el) el.scrollTop = el.scrollHeight;
      }, 60);
    });
  ")),

  tags$h3("Federated Statistics — Site Server",
          uiOutput("status_badge", inline = TRUE)),

  uiOutput("warn_ui"),
  uiOutput("address_ui"),

  div(class = "privacy",
      "\U0001f512  No individual patient records will leave this computer. ",
      "Only aggregate statistics (counts, means, model summaries) are shared with the coordinator."
  ),

  hr(),
  div(class = "sec-lbl", "Configuration"),

  fluidRow(
    column(5, uiOutput("file_ui")),
    column(3, numericInput("port", "Port", value = 8000, min = 1, max = 65535, step = 1)),
    column(4, passwordInput("token", "Security token", placeholder = "(leave blank if none)"))
  ),

  fluidRow(
    column(12, uiOutput("action_btn_ui"), style = "margin-top:10px;")
  ),

  hr(),
  div(class = "sec-lbl", "Server log"),
  verbatimTextOutput("server_log")
)

# -----------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------
server <- function(input, output, session) {

  rv <- reactiveValues(
    proc   = NULL,
    log    = character(0),
    status = "stopped"   # "stopped" | "starting" | "running"
  )

  timer <- reactiveTimer(500)

  # ---- Poll subprocess output every 500 ms -----------------------
  observe({
    timer()
    proc <- isolate(rv$proc)
    if (is.null(proc)) return()

    if (proc$is_alive()) {
      new_lines <- c(proc$read_output_lines(), proc$read_error_lines())
      if (length(new_lines)) {
        rv$log <- c(rv$log, new_lines)
        session$sendCustomMessage("scrollLog", list())
      }
      if (isolate(rv$status) == "starting" &&
          any(grepl("Running plumber|Listening|listening on|port",
                    isolate(rv$log), ignore.case = TRUE))) {
        rv$status <- "running"
      }
    } else {
      code    <- tryCatch(proc$get_exit_status(), error = function(e) NULL)
      new_err <- proc$read_error_lines()
      rv$log  <- c(rv$log, new_err,
                   sprintf("\n--- server exited (code %s) ---",
                           if (is.null(code)) "?" else as.character(code)))
      rv$status <- "stopped"
      rv$proc   <- NULL
      session$sendCustomMessage("scrollLog", list())
    }
  })

  # ---- Warnings --------------------------------------------------
  output$warn_ui <- renderUI({
    if (!.fedstats_ok)
      div(class = "warn-box",
          strong("Setup required: "),
          "The fedstats package is not installed. ",
          "Install it with: ",
          tags$code("devtools::install('fedstats')"),
          ", then restart this window.")
  })

  # ---- Status badge ----------------------------------------------
  output$status_badge <- renderUI({
    lbl <- switch(rv$status,
      stopped  = "Stopped",
      starting = "Starting…",
      running  = "Running"
    )
    span(class = paste0("bdg bdg-", rv$status), lbl)
  })

  # ---- Address display (only shown while running) ----------------
  output$address_ui <- renderUI({
    if (rv$status == "stopped") return(NULL)
    port <- isolate(input$port)
    if (nzchar(.ts_ip)) {
      div(class = "addr",
          div(class = "addr-lbl", "Tell the coordinator your address"),
          div(class = "addr-url", sprintf("http://%s:%d", .ts_ip, port)))
    } else {
      div(class = "addr",
          div(class = "addr-lbl", "Local address (Tailscale not detected)"),
          div(class = "addr-url", sprintf("http://localhost:%d", port)),
          div(style = "font-size:.82em; color:#666; margin-top:4px;",
              "The coordinator must be on the same machine or local network."))
    }
  })

  # ---- File selector (auto-detect or manual browse) --------------
  output$file_ui <- renderUI({
    if (length(.csv_files) > 0) {
      selectInput("data_file", "Data file",
                  choices = setNames(.csv_files, basename(.csv_files)))
    } else {
      tagList(
        div(style = "color:#c53030; font-size:.83em; margin-bottom:4px;",
            "No CSV found in data/ — select one manually:"),
        fileInput("data_file_upload", "Data file (.csv)",
                  accept = ".csv", buttonLabel = "Browse…",
                  placeholder = "No file selected")
      )
    }
  })

  # ---- Start / Stop button ---------------------------------------
  output$action_btn_ui <- renderUI({
    if (rv$status == "stopped") {
      actionButton("btn_start", "Start Server",
                   class = "btn btn-primary",
                   style = "min-width:140px; font-size:1em;")
    } else {
      actionButton("btn_stop", "Stop Server",
                   class = "btn btn-danger",
                   style = "min-width:140px; font-size:1em;")
    }
  })

  # ---- Start server ----------------------------------------------
  observeEvent(input$btn_start, {
    if (!.fedstats_ok) {
      showNotification("fedstats package not installed — see setup warning above.",
                       type = "error"); return()
    }

    data_path <- if (length(.csv_files) > 0) {
      input$data_file
    } else {
      req(input$data_file_upload)
      input$data_file_upload$datapath
    }

    if (is.null(data_path) || !nzchar(data_path) || !file.exists(data_path)) {
      showNotification("Data file not found. Please select a valid CSV.",
                       type = "error"); return()
    }

    port  <- as.integer(input$port)
    token <- trimws(input$token)

    env_vars                  <- Sys.getenv()
    env_vars["FED_DATA_FILE"] <- data_path
    env_vars["FED_PORT"]      <- as.character(port)
    if (nzchar(token)) env_vars["FED_TOKEN"] <- token

    rv$log    <- character(0)
    rv$status <- "starting"

    rv$proc <- tryCatch(
      processx::process$new(
        "Rscript", args = .api_script,
        wd     = .app_root,
        env    = env_vars,
        stdout = "|",
        stderr = "|"
      ),
      error = function(e) {
        rv$status <- "stopped"
        showNotification(paste("Failed to start server:", conditionMessage(e)),
                         type = "error", duration = 10)
        NULL
      }
    )
  })

  # ---- Stop server -----------------------------------------------
  observeEvent(input$btn_stop, {
    proc <- rv$proc
    if (!is.null(proc) && proc$is_alive()) {
      proc$kill()
      rv$log <- c(rv$log, "\n--- server stopped by user ---")
    }
    rv$status <- "stopped"
    rv$proc   <- NULL
    session$sendCustomMessage("scrollLog", list())
  })

  # ---- Log display -----------------------------------------------
  output$server_log <- renderText({
    if (!length(rv$log))
      return("Server log will appear here once the server starts…")
    paste(rv$log, collapse = "\n")
  })

  # ---- Kill subprocess on browser close --------------------------
  session$onSessionEnded(function() {
    proc <- isolate(rv$proc)
    if (!is.null(proc) && proc$is_alive()) proc$kill()
  })
}

shinyApp(ui, server)
