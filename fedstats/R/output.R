# Package-level environment for storing registered outputs.
# Each call to register_output() appends to .fed_state$outputs.
.fed_state <- new.env(parent = emptyenv())
.fed_state$outputs <- list()

#' Register an analysis output for the Shiny coordinator GUI
#'
#' Analysis scripts call \code{register_output()} to declare results they
#' want displayed. The GUI collects all registered outputs and renders each
#' one as a dynamically-created tab.
#'
#' @param name Character. Display name shown as the tab label.
#' @param value The output to register:
#'   \itemize{
#'     \item \strong{table}: a \code{data.frame}.
#'     \item \strong{plot}: a zero-argument \code{function} that draws a
#'       base-R plot, \emph{or} a \code{ggplot} object.
#'     \item \strong{text}: a character string (rendered verbatim).
#'   }
#' @param type Character. One of \code{"table"}, \code{"plot"},
#'   \code{"text"}.
#' @param caption Optional character. A short caption/footnote shown below
#'   the output.
#'
#' @return \code{NULL} invisibly. Called for its side effect.
#'
#' @examples
#' \dontrun{
#' # Table
#' register_output("Age summary", age_df, type = "table")
#'
#' # Base-R plot (pass a function)
#' register_output("Forest plot", function() {
#'   plot(or, seq_along(or), xlab = "OR", yaxt = "n")
#' }, type = "plot")
#'
#' # Text
#' register_output("Model fit", sprintf("N = %d, AIC = %.1f", n, aic),
#'                 type = "text")
#' }
#'
#' @export
register_output <- function(name, value, type = c("table", "plot", "text"),
                             caption = NULL) {
  type <- match.arg(type)
  .fed_state$outputs <- c(
    .fed_state$outputs,
    list(list(name = name, value = value, type = type, caption = caption))
  )
  invisible(NULL)
}

#' Retrieve all registered outputs
#'
#' Returns the list accumulated by \code{\link{register_output}} calls.
#' Primarily used internally by the Shiny coordinator GUI.
#'
#' @return A list of output descriptors, each with elements \code{name},
#'   \code{value}, \code{type}, \code{caption}.
#'
#' @export
get_outputs <- function() {
  .fed_state$outputs
}

#' Clear all registered outputs
#'
#' Resets the output registry. Called by the GUI before sourcing an
#' analysis script so stale outputs from a previous run are not shown.
#'
#' @return \code{NULL} invisibly.
#'
#' @export
clear_outputs <- function() {
  .fed_state$outputs <- list()
  invisible(NULL)
}
