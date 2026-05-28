# Internal helpers ---------------------------------------------------

.formula_to_string <- function(formula) {
  paste(deparse(formula), collapse = "")
}

.payload_to_matrix <- function(x) {
  M <- matrix(
    as.numeric(unlist(x$data, use.names = FALSE)),
    nrow = as.integer(x$nrow),
    ncol = as.integer(x$ncol)
  )
  if (!is.null(x$rownames)) rownames(M) <- unlist(x$rownames)
  if (!is.null(x$colnames)) colnames(M) <- unlist(x$colnames)
  M
}

.remote_post <- function(base_url, path, body = list(), token = NULL) {
  url <- paste0(sub("/+$", "", base_url), path)

  headers <- if (!is.null(token) && nzchar(token)) {
    httr::add_headers(
      `Content-Type`  = "application/json",
      `Authorization` = paste("Bearer", token)
    )
  } else {
    httr::add_headers(`Content-Type` = "application/json")
  }

  res <- httr::POST(
    url,
    headers,
    body   = jsonlite::toJSON(body, auto_unbox = TRUE, null = "null"),
    encode = "raw"
  )

  if (httr::status_code(res) >= 300) {
    stop(sprintf(
      "Remote call failed [%d] %s\n%s",
      httr::status_code(res), url,
      httr::content(res, as = "text", encoding = "UTF-8")
    ))
  }

  httr::content(res, as = "parsed", type = "application/json")
}

# Exported -----------------------------------------------------------

#' Create a remote site server adapter
#'
#' Returns a server object whose interface is identical to
#' \code{\link{create_server}} but routes every call over HTTP to a
#' site running \code{api_server.R}. All \code{fed_*()} functions work
#' transparently with either in-process or remote servers.
#'
#' @param base_url Base URL of the site, e.g.
#'   \code{"http://100.74.226.3:8000"}.
#' @param token Optional bearer token. Defaults to the environment
#'   variable \code{FED_API_TOKEN}.
#'
#' @return A named list of functions matching the interface of
#'   \code{\link{create_server}}.
#'
#' @examples
#' \dontrun{
#' srv <- create_remote_server("http://100.74.226.3:8000", token = "secret")
#' srv$summary_numeric("age")
#' }
#'
#' @export
create_remote_server <- function(base_url,
                                 token = Sys.getenv("FED_API_TOKEN")) {
  list(

    termnames = function(formula) {
      r <- .remote_post(base_url, "/termnames",
                        list(formula = .formula_to_string(formula)), token)
      unlist(r$termnames)
    },

    grad_hess = function(formula, beta) {
      r <- .remote_post(base_url, "/grad_hess",
                        list(formula    = .formula_to_string(formula),
                             beta       = as.list(as.numeric(beta)),
                             beta_names = as.list(names(beta))),
                        token)
      grad <- as.numeric(unlist(r$grad, use.names = FALSE))
      names(grad) <- unlist(r$termnames)
      list(n    = as.integer(r$n),
           grad = grad,
           hess = .payload_to_matrix(r$hess),
           ll   = as.numeric(r$ll))
    },

    lm_suffstats = function(formula) {
      r <- .remote_post(base_url, "/lm_suffstats",
                        list(formula = .formula_to_string(formula)), token)
      Xty <- as.numeric(unlist(r$Xty, use.names = FALSE))
      names(Xty) <- unlist(r$termnames)
      list(n         = as.integer(r$n),
           termnames = unlist(r$termnames),
           XtX       = .payload_to_matrix(r$XtX),
           Xty       = Xty,
           yTy       = as.numeric(r$yTy))
    },

    summary_numeric = function(varname) {
      r <- .remote_post(base_url, "/summary_numeric",
                        list(varname = varname), token)
      list(type  = r$type,
           n     = as.integer(r$n),
           sum   = as.numeric(r$sum),
           sumsq = as.numeric(r$sumsq))
    },

    group_summaries = function(varname, groupvar) {
      r <- .remote_post(base_url, "/group_summaries",
                        list(varname = varname, groupvar = groupvar), token)
      stats <- lapply(r$stats, function(z) {
        list(n = as.integer(z$n), sum = as.numeric(z$sum), sumsq = as.numeric(z$sumsq))
      })
      list(type = r$type, stats = stats)
    },

    counts_2x2 = function(xvar, yvar) {
      r <- .remote_post(base_url, "/counts_2x2",
                        list(xvar = xvar, yvar = yvar), token)
      list(type = r$type,
           n00  = as.integer(r$n00), n01 = as.integer(r$n01),
           n10  = as.integer(r$n10), n11 = as.integer(r$n11),
           n    = as.integer(r$n))
    },

    validate_data = function(vars_spec, formula = NULL, min_n = 20L) {
      .remote_post(
        base_url, "/validate",
        list(vars_spec = vars_spec,
             formula   = if (!is.null(formula)) .formula_to_string(formula) else NULL,
             min_n     = as.integer(min_n)),
        token
      )
    }
  )
}
