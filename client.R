# client.R
# =====================================================================
# Federated Statistics - Coordinator Client
#
# Two modes:
#   In-process : use servers created with create_server() from server.R
#   Remote     : use servers created with create_remote_server(url)
#
# All fed_* functions work identically in both modes.
# =====================================================================

.formula_to_string <- function(formula) {
  paste(deparse(formula), collapse = "")
}

.matrix_to_payload <- function(M) {
  list(
    data     = as.numeric(M),
    nrow     = nrow(M),
    ncol     = ncol(M),
    rownames = rownames(M),
    colnames = colnames(M)
  )
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

# ------------------------------------------------------------------
# Low-level HTTP POST helper
# ------------------------------------------------------------------
.remote_post <- function(base_url, path, body = list(), token = NULL) {
  if (!requireNamespace("httr",     quietly = TRUE))
    stop("Package 'httr' is required. Install with install.packages('httr').")
  if (!requireNamespace("jsonlite", quietly = TRUE))
    stop("Package 'jsonlite' is required. Install with install.packages('jsonlite').")
  
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

# ------------------------------------------------------------------
# Remote server adapter  (mirrors the interface of create_server())
# ------------------------------------------------------------------
create_remote_server <- function(base_url,
                                 token = Sys.getenv("FED_API_TOKEN")) {
  list(
    
    termnames = function(formula) {
      r <- .remote_post(
        base_url, "/termnames",
        list(formula = .formula_to_string(formula)),
        token
      )
      unlist(r$termnames)
    },
    
    grad_hess = function(formula, beta) {
      r <- .remote_post(
        base_url, "/grad_hess",
        list(
          formula    = .formula_to_string(formula),
          beta       = as.list(as.numeric(beta)),
          beta_names = as.list(names(beta))
        ),
        token
      )
      
      grad <- as.numeric(unlist(r$grad, use.names = FALSE))
      names(grad) <- unlist(r$termnames)
      
      list(
        n    = as.integer(r$n),
        grad = grad,
        hess = .payload_to_matrix(r$hess),
        ll   = as.numeric(r$ll)
      )
    },
    
    lm_suffstats = function(formula) {
      r <- .remote_post(
        base_url, "/lm_suffstats",
        list(formula = .formula_to_string(formula)),
        token
      )
      
      Xty <- as.numeric(unlist(r$Xty, use.names = FALSE))
      names(Xty) <- unlist(r$termnames)
      
      list(
        n         = as.integer(r$n),
        termnames = unlist(r$termnames),
        XtX       = .payload_to_matrix(r$XtX),
        Xty       = Xty,
        yTy       = as.numeric(r$yTy)
      )
    },
    
    summary_numeric = function(varname) {
      r <- .remote_post(
        base_url, "/summary_numeric",
        list(varname = varname),
        token
      )
      list(
        type  = r$type,
        n     = as.integer(r$n),
        sum   = as.numeric(r$sum),
        sumsq = as.numeric(r$sumsq)
      )
    },
    
    group_summaries = function(varname, groupvar) {
      r <- .remote_post(
        base_url, "/group_summaries",
        list(varname = varname, groupvar = groupvar),
        token
      )
      
      stats <- lapply(r$stats, function(z) {
        list(n = as.integer(z$n), sum = as.numeric(z$sum), sumsq = as.numeric(z$sumsq))
      })
      
      list(type = r$type, stats = stats)
    },
    
    counts_2x2 = function(xvar, yvar) {
      r <- .remote_post(
        base_url, "/counts_2x2",
        list(xvar = xvar, yvar = yvar),
        token
      )
      list(
        type = r$type,
        n00  = as.integer(r$n00), n01 = as.integer(r$n01),
        n10  = as.integer(r$n10), n11 = as.integer(r$n11),
        n    = as.integer(r$n)
      )
    },

    validate_data = function(vars_spec, formula = NULL, min_n = 20L) {
      .remote_post(
        base_url, "/validate",
        list(
          vars_spec = vars_spec,
          formula   = if (!is.null(formula)) .formula_to_string(formula) else NULL,
          min_n     = as.integer(min_n)
        ),
        token
      )
    }
  )
}

# ==================================================================
# Federated analysis functions (identical for local and remote use)
# ==================================================================

fed_numeric <- function(servers, varname) {
  n <- 0; s <- 0; ss <- 0
  for (srv in servers) {
    r  <- srv$summary_numeric(varname)
    n  <- n  + r$n
    s  <- s  + r$sum
    ss <- ss + r$sumsq
  }
  list(n = n, mean = s / n, sd = sqrt((ss - s^2 / n) / (n - 1)),
       sum = s)     # sum exposed so run.R can compute MCID count
}

fed_welch_t <- function(servers, varname, groupvar, group1, group2) {
  n1 <- s1 <- ss1 <- 0
  n2 <- s2 <- ss2 <- 0
  
  for (srv in servers) {
    stats <- srv$group_summaries(varname, groupvar)$stats
    
    a <- stats[[as.character(group1)]]
    b <- stats[[as.character(group2)]]
    
    if (!is.null(a)) { n1 <- n1 + a$n; s1 <- s1 + a$sum; ss1 <- ss1 + a$sumsq }
    if (!is.null(b)) { n2 <- n2 + b$n; s2 <- s2 + b$sum; ss2 <- ss2 + b$sumsq }
  }
  
  mean1 <- s1 / n1;  mean2 <- s2 / n2
  var1  <- (ss1 - s1^2 / n1) / (n1 - 1)
  var2  <- (ss2 - s2^2 / n2) / (n2 - 1)
  
  se <- sqrt(var1 / n1 + var2 / n2)
  t  <- (mean1 - mean2) / se
  df <- (var1 / n1 + var2 / n2)^2 /
    ((var1 / n1)^2 / (n1 - 1) + (var2 / n2)^2 / (n2 - 1))
  p  <- 2 * (1 - pt(abs(t), df = df))
  
  list(n1 = n1, n2 = n2, mean1 = mean1, mean2 = mean2,
       sd1 = sqrt(var1), sd2 = sqrt(var2),
       t = t, df = df, p = p)
}

fed_chisq_2x2 <- function(servers, xvar, yvar, correct = TRUE) {
  n00 <- n01 <- n10 <- n11 <- 0
  
  for (srv in servers) {
    r   <- srv$counts_2x2(xvar, yvar)
    n00 <- n00 + r$n00; n01 <- n01 + r$n01
    n10 <- n10 + r$n10; n11 <- n11 + r$n11
  }
  
  tbl  <- matrix(c(n00, n01, n10, n11), nrow = 2, byrow = TRUE,
                 dimnames = list(paste0(xvar,  c("=0", "=1")),
                                 paste0(yvar, c("=0", "=1"))))
  test <- suppressWarnings(chisq.test(tbl, correct = correct))
  
  list(table = tbl,
       statistic = unname(test$statistic),
       df        = unname(test$parameter),
       p         = unname(test$p.value),
       N         = sum(tbl))
}

fed_lm <- function(servers, formula) {
  XtX <- NULL; Xty <- NULL; yTy <- 0; N <- 0; termnames <- NULL
  
  for (srv in servers) {
    r <- srv$lm_suffstats(formula)
    if (is.null(termnames)) termnames <- r$termnames
    if (is.null(XtX)) { XtX <- r$XtX; Xty <- r$Xty
    } else             { XtX <- XtX + r$XtX; Xty <- Xty + r$Xty }
    yTy <- yTy + r$yTy
    N   <- N   + r$n
  }
  
  beta <- as.vector(solve(XtX, Xty));  names(beta) <- termnames
  p    <- length(beta)
  RSS  <- yTy - 2 * sum(beta * Xty) + as.numeric(t(beta) %*% XtX %*% beta)
  sigma2 <- RSS / (N - p)
  vcov   <- sigma2 * solve(XtX)
  se     <- sqrt(diag(vcov))
  tstat  <- beta / se
  pval   <- 2 * (1 - pt(abs(tstat), df = N - p))
  
  list(coefficients = beta, se = se, t = tstat, p = pval,
       sigma = sqrt(sigma2), df = N - p, N = N, vcov = vcov)
}

fed_logistic_newton <- function(servers, formula,
                                max_iter       = 50,
                                tol_score      = 1e-8,
                                tol_loglik     = 1e-10,
                                robust_cluster = TRUE,
                                verbose        = FALSE) {
  
  termnames <- servers[[1]]$termnames(formula)
  p    <- length(termnames)
  beta <- setNames(rep(0, p), termnames)
  
  federated_loglik <- function(b) {
    total <- 0
    for (srv in servers) total <- total + srv$grad_hess(formula, b)$ll
    total
  }
  
  loglik <- federated_loglik(beta)
  iter   <- 0L
  converged <- FALSE
  
  for (iter in seq_len(max_iter)) {
    grad <- rep(0, p);  hess <- matrix(0, p, p)
    for (srv in servers) {
      r    <- srv$grad_hess(formula, beta)
      grad <- grad + r$grad
      hess <- hess + r$hess
    }
    
    score_norm <- sqrt(sum(grad^2))
    if (verbose) cat(sprintf("Newton iter %02d | score norm = %.3e\n", iter, score_norm))
    if (score_norm < tol_score) { converged <- TRUE; break }
    
    beta_new   <- beta - solve(hess, grad);  names(beta_new) <- termnames
    loglik_new <- federated_loglik(beta_new)
    
    if (abs(loglik_new - loglik) < tol_loglik) {
      beta <- beta_new; loglik <- loglik_new; converged <- TRUE; break
    }
    beta <- beta_new; loglik <- loglik_new
  }
  
  hess        <- matrix(0, p, p)
  site_scores <- vector("list", length(servers))
  N           <- 0
  
  for (i in seq_along(servers)) {
    r              <- servers[[i]]$grad_hess(formula, beta)
    hess           <- hess + r$hess
    site_scores[[i]] <- as.numeric(r$grad)
    N              <- N + r$n
  }
  
  vcov <- solve(-hess);  se <- sqrt(diag(vcov))
  
  out <- list(coefficients = beta, se = se, vcov = vcov,
              logLik = federated_loglik(beta),
              N = N, iterations = iter, converged = converged)
  
  if (robust_cluster && length(servers) >= 2) {
    G    <- length(servers)
    meat <- matrix(0, p, p)
    for (u in site_scores) meat <- meat + tcrossprod(u)
    cr1           <- (G / (G - 1)) * ((N - 1) / (N - p))
    vcov_robust   <- cr1 * (vcov %*% meat %*% vcov)
    out$vcov_robust <- vcov_robust
    out$se_robust   <- sqrt(diag(vcov_robust))
    out$clusters    <- G
  }
  out
}

# ==================================================================
# Federated data validation
# ==================================================================

# fed_validate() — collect per-site validation reports and compute
# cross-site heterogeneity (I²) for each numeric/binary variable.
#
# vars_spec : named list describing expected variables (see server.R)
# formula   : optional formula for complete-case feasibility check
# min_n     : minimum complete cases required per site for the formula
# i2_warn   : I² threshold (%) above which a heterogeneity warning is issued
#
# Returns a list:
#   ok           — TRUE iff no errors were found
#   errors       — character vector of blocking problems
#   warnings     — character vector of non-blocking concerns
#   site_reports — raw per-site validate_data() outputs
#   heterogeneity — named list of I² statistics per numeric/binary variable
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

  # Cross-site heterogeneity (I²) for numeric and binary variables.
  # Uses inverse-variance weighting on per-site means.
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

    m_ok <- means[ok];  s_ok <- sds[ok];  n_ok <- ns[ok]
    w    <- n_ok / s_ok^2              # inverse-variance weights
    mu   <- sum(w * m_ok) / sum(w)    # pooled mean
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


# print_validation_report() — human-readable summary of fed_validate() output.
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
    if (n_w == 0)
      cat("Validation: PASS\n")
    else
      cat(sprintf("Validation: PASS  (%d warning(s))\n", n_w))
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