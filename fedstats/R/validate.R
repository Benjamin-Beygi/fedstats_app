#' Federated data validation
#'
#' Collects per-site variable reports and computes cross-site heterogeneity
#' (I²) for numeric and binary variables. Should be called before running
#' any analysis to catch missing columns, wrong types, out-of-range values,
#' and formula feasibility issues.
#'
#' @param servers List of server objects (in-process or remote).
#' @param vars_spec Named list describing expected variables. Each element is
#'   a list with:
#'   \itemize{
#'     \item \code{type}: \code{"numeric"}, \code{"binary"}, or
#'       \code{"categorical"}.
#'     \item \code{min}, \code{max}: (numeric, optional) expected range for
#'       numeric/binary variables.
#'     \item \code{levels}: (character vector, optional) expected factor
#'       levels for categorical variables.
#'   }
#' @param formula Optional R formula. If supplied, each site checks whether
#'   there are enough complete cases to fit the model.
#' @param min_n Integer. Minimum acceptable complete-case count per site
#'   (default \code{20L}). Sites with fewer cases receive a warning (not
#'   an error) unless the count is zero.
#' @param i2_warn Numeric. I² threshold above which a cross-site
#'   heterogeneity warning is issued (default \code{75}).
#'
#' @return A list with:
#'   \itemize{
#'     \item \code{ok}: \code{TRUE} iff no errors were found.
#'     \item \code{errors}: character vector of blocking problems.
#'     \item \code{warnings}: character vector of non-blocking concerns.
#'     \item \code{site_reports}: raw per-site \code{validate_data()} outputs.
#'     \item \code{heterogeneity}: named list of I² statistics per variable.
#'   }
#'
#' @export
fed_validate <- function(servers, vars_spec, formula = NULL,
                         min_n = 20L, i2_warn = 75) {

  site_reports <- lapply(seq_along(servers), function(i) {
    tryCatch(
      servers[[i]]$validate_data(vars_spec, formula, min_n),
      error = function(e) list(site_error = conditionMessage(e))
    )
  })

  errors   <- character(0)
  warnings <- character(0)

  for (i in seq_along(site_reports)) {
    rpt   <- site_reports[[i]]
    label <- paste0("Site ", i)

    if (!is.null(rpt$site_error)) {
      errors <- c(errors, paste0(label, " unreachable: ", rpt$site_error))
      next
    }

    for (vname in names(rpt$var_reports)) {
      vrep <- rpt$var_reports[[vname]]
      pfx  <- paste0(label, " / ", vname, ": ")

      if (!isTRUE(unlist(vrep$exists)))
        errors <- c(errors, paste0(pfx, "variable not found in dataset."))
      if (!is.null(vrep$binary_error))
        errors <- c(errors, paste0(pfx, vrep$binary_error))
      if (!is.null(vrep$type_warning))
        warnings <- c(warnings, paste0(pfx, vrep$type_warning))
      if (!is.null(vrep$range_warning))
        warnings <- c(warnings, paste0(pfx, vrep$range_warning))
      if (!is.null(vrep$level_warning))
        warnings <- c(warnings, paste0(pfx, vrep$level_warning))
      if (isTRUE(as.numeric(vrep$pct_missing) > 50))
        warnings <- c(warnings, paste0(pfx, vrep$pct_missing, "% missing."))
    }

    frep <- rpt$formula_report
    if (!is.null(frep)) {
      if (!isTRUE(unlist(frep$feasible))) {
        msg <- if (!is.null(frep$error)) frep$error else
          "0 complete cases — site cannot contribute to this model."
        errors <- c(errors, paste0(label, " (formula): ", msg))
      } else if (isTRUE(unlist(frep$low_n))) {
        warnings <- c(warnings, sprintf(
          "%s (formula): only %d complete cases (recommended minimum: %d).",
          label, as.integer(unlist(frep$n_complete)), as.integer(min_n)
        ))
      }
    }
  }

  # Cross-site heterogeneity (I²) for numeric/binary variables
  heterogeneity <- list()
  numeric_vars  <- names(vars_spec)[
    sapply(vars_spec, function(s) s$type %in% c("numeric", "binary"))
  ]

  for (vname in numeric_vars) {
    get_field <- function(field)
      vapply(site_reports, function(rpt) {
        v <- rpt$var_reports[[vname]]
        if (is.null(v) || !isTRUE(unlist(v$exists)) || is.null(v[[field]])) NA_real_
        else as.numeric(v[[field]])
      }, numeric(1))

    means <- get_field("mean")
    sds   <- get_field("sd")
    ns    <- get_field("n_valid")

    ok <- !is.na(means) & !is.na(sds) & !is.na(ns) & ns > 1 & sds > 0
    if (sum(ok) < 2) next

    m_ok <- means[ok]; s_ok <- sds[ok]; n_ok <- ns[ok]
    w    <- n_ok / s_ok^2
    mu   <- sum(w * m_ok) / sum(w)
    Q    <- sum(w * (m_ok - mu)^2)
    df_h <- sum(ok) - 1
    i2   <- max(0, (Q - df_h) / Q) * 100

    heterogeneity[[vname]] <- list(
      i2          = round(i2, 1),
      Q           = round(Q, 3),
      df          = df_h,
      pooled_mean = round(mu, 3),
      site_means  = round(means, 3)
    )

    if (i2 > i2_warn)
      warnings <- c(warnings, sprintf(
        "%s: high cross-site heterogeneity (I² = %.1f%%).", vname, i2
      ))
  }

  list(ok = length(errors) == 0, errors = errors, warnings = warnings,
       site_reports = site_reports, heterogeneity = heterogeneity)
}

#' Print a federated validation report
#'
#' Formats the output of \code{\link{fed_validate}} for console display,
#' showing per-site status, errors, warnings, and the I² heterogeneity
#' table.
#'
#' @param vr Return value from \code{\link{fed_validate}}.
#'
#' @return \code{vr} invisibly (for piping).
#'
#' @export
print_validation_report <- function(vr) {
  cat("=====================================================\n")
  cat(" Federated Data Validation\n")
  cat("=====================================================\n")

  for (i in seq_along(vr$site_reports)) {
    rpt <- vr$site_reports[[i]]

    if (!is.null(rpt$site_error)) {
      cat(sprintf("Site %-2d [ERROR]   unreachable: %s\n", i, rpt$site_error))
      next
    }

    errs  <- character(0)
    warns <- character(0)
    for (vname in names(rpt$var_reports)) {
      vrep <- rpt$var_reports[[vname]]
      if (!isTRUE(unlist(vrep$exists)))
        errs  <- c(errs,  paste0(vname, ": not found"))
      if (!is.null(vrep$binary_error))
        errs  <- c(errs,  paste0(vname, ": ", vrep$binary_error))
      if (!is.null(vrep$type_warning))
        warns <- c(warns, paste0(vname, ": ", vrep$type_warning))
      if (!is.null(vrep$range_warning))
        warns <- c(warns, paste0(vname, ": ", vrep$range_warning))
      if (!is.null(vrep$level_warning))
        warns <- c(warns, paste0(vname, ": ", vrep$level_warning))
      if (isTRUE(as.numeric(vrep$pct_missing) > 50))
        warns <- c(warns, paste0(vname, ": ", vrep$pct_missing, "% missing"))
    }

    status <- if (length(errs) > 0) "ERROR  " else
      if (length(warns) > 0) "WARNING" else "OK     "
    cat(sprintf("Site %-2d [%s]  n = %d\n", i, status,
                as.integer(rpt$n_rows)))
    for (e in errs)  cat(sprintf("  [ERROR]  %s\n", e))
    for (w in warns) cat(sprintf("  [WARN]   %s\n", w))

    frep <- rpt$formula_report
    if (!is.null(frep)) {
      if (!isTRUE(unlist(frep$feasible))) {
        msg <- if (!is.null(frep$error)) frep$error else
          "0 complete cases — cannot contribute."
        cat(sprintf("  [ERROR]  formula: %s\n", msg))
      } else if (isTRUE(unlist(frep$low_n))) {
        cat(sprintf("  [WARN]   formula: only %d complete cases\n",
                    as.integer(unlist(frep$n_complete))))
      } else {
        cat(sprintf("  formula: %d complete cases\n",
                    as.integer(unlist(frep$n_complete))))
      }
    }
  }

  if (length(vr$heterogeneity) > 0) {
    cat(sprintf("\nHeterogeneity across %d sites (I²):\n",
                length(vr$site_reports)))
    for (vname in names(vr$heterogeneity)) {
      h    <- vr$heterogeneity[[vname]]
      flag <- if (h$i2 > 75) "*** HIGH" else
        if (h$i2 > 50) "*   MOD." else "    LOW "
      site_str <- paste(
        ifelse(is.na(h$site_means), "NA", sprintf("%.2f", h$site_means)),
        collapse = ", "
      )
      cat(sprintf("  %-22s I² = %5.1f%%  %s  means: [%s]\n",
                  paste0(vname, ":"), h$i2, flag, site_str))
    }
  }

  cat("\n")
  if (vr$ok) {
    n_w <- length(vr$warnings)
    if (n_w == 0) cat("Validation: PASS\n")
    else          cat(sprintf("Validation: PASS  (%d warning(s))\n", n_w))
    if (n_w > 0) for (w in vr$warnings) cat(sprintf("  [WARN] %s\n", w))
  } else {
    cat(sprintf("Validation: FAIL  (%d error(s), %d warning(s))\n",
                length(vr$errors), length(vr$warnings)))
    for (e in vr$errors)   cat(sprintf("  [ERROR] %s\n", e))
    for (w in vr$warnings) cat(sprintf("  [WARN]  %s\n", w))
  }
  cat("=====================================================\n")
  invisible(vr)
}
