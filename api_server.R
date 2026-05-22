# api_server.R
# =====================================================================
# Federated Statistics - Site HTTP API (run at each hospital)
#
# Usage:
#   Rscript api_server.R --data path/to/site_data.csv --port 8000
#
# Or set environment variables:
#   FED_DATA_FILE=path/to/data.csv
#   FED_PORT=8000
#   FED_TOKEN=mysecrettoken   (optional; coordinator must send same token)
#   FED_MIN_N=20              (refuse queries with fewer rows than this)
# =====================================================================

suppressPackageStartupMessages({
  library(plumber)
  library(jsonlite)
})

source("server.R")   # provides create_server()

# ------------------------------------------------------------------
# Configuration: CLI args override env vars
# ------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)

get_arg <- function(flag, env_var, default = NULL) {
  idx <- which(args == flag)
  if (length(idx) && length(args) >= idx + 1) return(args[idx + 1])
  val <- Sys.getenv(env_var, unset = "")
  if (nzchar(val)) return(val)
  default
}

DATA_FILE <- get_arg("--data",  "FED_DATA_FILE", NULL)
PORT      <- as.integer(get_arg("--port",  "FED_PORT",  "8000"))
TOKEN     <- get_arg("--token", "FED_TOKEN", "")        # "" = no auth
MIN_N     <- as.integer(get_arg("--min-n", "FED_MIN_N", "20"))

if (is.null(DATA_FILE) || !nzchar(DATA_FILE)) {
  stop("Provide --data <csv_path> or set FED_DATA_FILE.")
}
if (!file.exists(DATA_FILE)) {
  stop("Data file not found: ", DATA_FILE)
}

cat(sprintf("[api_server] Loading data: %s\n", DATA_FILE))
site_data <- read.csv(
  DATA_FILE,
  stringsAsFactors = FALSE,
  na.strings = c("", "NA", "NaN", "NULL")
)

srv <- create_server(site_data, min_n = MIN_N)
cat(sprintf("[api_server] Loaded %d rows. Listening on port %d.\n",
            nrow(site_data), PORT))

# ------------------------------------------------------------------
# Helper: check bearer token if one is configured
# ------------------------------------------------------------------
check_token <- function(req) {
  if (!nzchar(TOKEN)) return(invisible(NULL))   # auth disabled
  auth <- req$HTTP_AUTHORIZATION
  if (is.null(auth) || !grepl("^Bearer ", auth)) {
    stop("Unauthorized: missing or malformed Authorization header.")
  }
  if (sub("^Bearer ", "", auth) != TOKEN) {
    stop("Unauthorized: invalid token.")
  }
  invisible(NULL)
}

# ------------------------------------------------------------------
# Plumber API definition
# ------------------------------------------------------------------
#* @apiTitle Federated Statistics Site API
#* @apiDescription Exposes local aggregate statistics for federated GLM.

pr <- plumber::Plumber$new()

# ---- /termnames ------------------------------------------------
pr$handle("POST", "/termnames", function(req, res) {
  tryCatch({
    check_token(req)
    body    <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)
    formula <- as.formula(body$formula)
    list(termnames = as.list(srv$termnames(formula)))
  }, error = function(e) {
    res$status <- 400
    list(error = conditionMessage(e))
  })
})

# ---- /grad_hess ------------------------------------------------
pr$handle("POST", "/grad_hess", function(req, res) {
  tryCatch({
    check_token(req)
    body      <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)
    formula   <- as.formula(body$formula)
    beta_vals <- as.numeric(unlist(body$beta, use.names = FALSE))
    beta_names <- unlist(body$beta_names)
    beta      <- setNames(beta_vals, beta_names)
    
    r <- srv$grad_hess(formula, beta)
    
    # Serialise matrix as flat vector + dims
    list(
      n         = r$n,
      termnames = as.list(names(r$grad)),
      grad      = as.list(as.numeric(r$grad)),
      hess      = list(
        data    = as.list(as.numeric(r$hess)),
        nrow    = nrow(r$hess),
        ncol    = ncol(r$hess),
        rownames = as.list(rownames(r$hess)),
        colnames = as.list(colnames(r$hess))
      ),
      ll = r$ll
    )
  }, error = function(e) {
    res$status <- 400
    list(error = conditionMessage(e))
  })
})

# ---- /lm_suffstats ---------------------------------------------
pr$handle("POST", "/lm_suffstats", function(req, res) {
  tryCatch({
    check_token(req)
    body    <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)
    formula <- as.formula(body$formula)
    
    r <- srv$lm_suffstats(formula)
    
    list(
      n         = r$n,
      termnames = as.list(r$termnames),
      XtX       = list(
        data    = as.list(as.numeric(r$XtX)),
        nrow    = nrow(r$XtX),
        ncol    = ncol(r$XtX),
        rownames = as.list(rownames(r$XtX)),
        colnames = as.list(colnames(r$XtX))
      ),
      Xty = as.list(as.numeric(r$Xty)),
      yTy = r$yTy
    )
  }, error = function(e) {
    res$status <- 400
    list(error = conditionMessage(e))
  })
})

# ---- /summary_numeric ------------------------------------------
pr$handle("POST", "/summary_numeric", function(req, res) {
  tryCatch({
    check_token(req)
    body    <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)
    r <- srv$summary_numeric(body$varname)
    list(type = r$type, n = r$n, sum = r$sum, sumsq = r$sumsq)
  }, error = function(e) {
    res$status <- 400
    list(error = conditionMessage(e))
  })
})

# ---- /group_summaries ------------------------------------------
pr$handle("POST", "/group_summaries", function(req, res) {
  tryCatch({
    check_token(req)
    body <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)
    r    <- srv$group_summaries(body$varname, body$groupvar)
    
    # Convert each group's stats list to JSON-safe form
    stats_out <- lapply(r$stats, function(z) {
      list(n = z$n, sum = z$sum, sumsq = z$sumsq)
    })
    
    list(type = r$type, stats = stats_out)
  }, error = function(e) {
    res$status <- 400
    list(error = conditionMessage(e))
  })
})

# ---- /counts_2x2 -----------------------------------------------
pr$handle("POST", "/counts_2x2", function(req, res) {
  tryCatch({
    check_token(req)
    body <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)
    r    <- srv$counts_2x2(body$xvar, body$yvar)
    list(
      type = r$type,
      n00  = r$n00, n01 = r$n01,
      n10  = r$n10, n11 = r$n11,
      n    = r$n
    )
  }, error = function(e) {
    res$status <- 400
    list(error = conditionMessage(e))
  })
})

# ---- /validate -------------------------------------------------
# Uses auto_unbox=TRUE so boolean/scalar fields arrive as plain JSON
# values (true, 6680) rather than single-element arrays ([true], [6680]).
# Plumber's default serializer wraps scalars in arrays, which breaks
# isTRUE() checks on the coordinator side after httr parses with
# simplifyVector=FALSE.
pr$handle("POST", "/validate", function(req, res) {
  tryCatch({
    check_token(req)
    body      <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)
    vars_spec <- body$vars_spec
    formula   <- if (!is.null(body$formula) && nzchar(body$formula))
                   as.formula(body$formula) else NULL
    min_n     <- if (!is.null(body$min_n)) as.integer(body$min_n) else 20L
    srv$validate_data(vars_spec, formula, min_n)
  }, error = function(e) {
    res$status <- 400
    list(error = conditionMessage(e))
  })
}, serializer = plumber::serializer_json(auto_unbox = TRUE, null = "null", na = "null"))

# ---- /health (simple ping) -------------------------------------
pr$handle("GET", "/health", function(req, res) {
  list(status = "ok", rows = nrow(site_data))
})

# ------------------------------------------------------------------
# Start
# ------------------------------------------------------------------
pr$run(host = "0.0.0.0", port = PORT)