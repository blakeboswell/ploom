
#' Linear model that uses only `p^2` memory for `p` variables.
#' 
#' @param data an optional data frame, list or environment
#'   (or object coercible by as.data.frame to a data frame)
#' @param formula A model formula: a symbolic description of the
#'   model to be fitted.
#' @param weights A one-sided, single term formula specifying weights
#' @param sandwich TRUE to compute the Huber/White sandwich covariance matrix
#'   (uses `p^4` memory rather than `p^2`)
#' @details The model formula must not contain any data-dependent terms, as
#'   these will not be consistent when updated. Factors are permitted, but 
#'   the levels of the factor must be the same across all data chunks 
#'   (empty factor levels are ok). Offsets are allowed.
#' @export
online_lm <- function(data,
                      formula,
                      weights  = NULL,
                      sandwich = FALSE,
                      ...) {

  model_terms <- terms(formula)
  batch_data  <- model.frame(model_terms, data)
  
  if(is.null(offset <- model.offset(batch_data))) {
    offset <- 0
  }
  
  batch_response <- model.response(batch_data) - offset
  batch_data     <- model.matrix(model_terms, batch_data)
  
  p <- ncol(batch_data)
  n <- nrow(batch_data) 

  if(is.null(weights)) {
    batch_weights <- rep(1.0, n)
  } else {
    if(!inherits(weights, "formula")) {
      stop("`weights` must be a formula")
    }
    batch_weights <- model.frame(weights, data)[[1]]
  }
  
  model_qr <- update(new_bounded_qr(p),
                     batch_data,
                     batch_response,
                     batch_weights)

  rval <- list(
    call     = sys.call(),
    qr       = model_qr,
    assign   = attr(batch_data, "assign"),
    terms    = model_terms,
    n        = n,
    names    = colnames(batch_data),
    weights  = weights,
    df.resid = n - p
  )

  if (sandwich) {
    xx        <- matrix(nrow = n, ncol = p * (p + 1))
    xx[, 1:p] <- batch_data * batch_response
    for (i in 1:p) {
      xx[, p * i + (1:p)] <- batch_data * batch_data[, i]
    }
    
    sandwich_qr <- update(new_bounded_qr(p * (p + 1)),
                          xx, rep(0, n), batch_weights ^ 2)
    rval$sandwich <- list(xy = sandwich_qr)
  }

  class(rval) <- "online_lm"
  rval

}


#' fit `online_lm` model to new batch of observations
#'
#' @param data an optional data frame, list or environment
#'   (or object coercible by as.data.frame to a data frame)
#' @param object online_lm object
#' @export
update_online_lm <- function(data, obj, ...) {

  batch_data  <- model.frame(obj$terms, data)
  
  if(is.null(offset <- model.offset(batch_data))) {
    offset <- 0
  }
  
  batch_response <- model.response(batch_data) - offset
  batch_data     <- model.matrix(obj$terms, batch_data)
  
  if(is.null(obj$weights)) {
    batch_weights <- NULL
  } else {
    batch_weights <- model.frame(obj$weights, data)[[1]]
  }
  
  if (!identical(obj$assign, attr(batch_data, "assign"))) {
    stop("model matrices incompatible")
  }

  update(obj$qr,
         batch_data,
         batch_response,
         batch_weights)
  
  obj$n <- obj$n + nrow(batch_data)

  if (!is.null(obj$sandwich)) {

    p <- ncol(batch_data)
    n <- nrow(batch_data) 
    
    xx <- matrix(nrow = n, ncol = p * (p + 1))
    xx[, 1:p] <- batch_data * batch_response
    for (i in 1:p) {
      xx[, p * i + (1:p)] <- batch_data * batch_data[, i]
    }
    sandwich_qr <- update(obj$sandwich$xy,
                          xx, rep(0, n), batch_weights ^ 2)
    obj$sandwich <- list(xy = sandwich_qr)
    
  }
  
  obj
  
}


#' @export
coef.online_lm <- function(obj, ...) {
  betas        <- coef(obj$qr)
  names(betas) <- obj$names
  betas
}


#' @export
vcov.online_lm <- function(obj, ...) {
  
  ## cpp implementation / HC not supported ---------------------------
  # V <- vcov(obj$qr)
  # dimnames(V) <- list(obj$names, obj$names)
  # 
  # V
  
  # biglm implementation --------------------------------------------
  
  nobs  <- obj$qr$nobs
  np    <- obj$qr$np
  sserr <- obj$qr$sserr
  ok    <- !obj$qr$lindep()
  
  R  <- rvcov_biglm(
    np,
    obj$qr$D,
    obj$qr$rbar,
    ok
  )
  
  dimnames(R) <- list(obj$names, obj$names)
  
  if(!is.null(obj$sandwich)) {
    
    betas <- coef(obj$qr)
    
    V <- sandwich_rcov_biglm(
      np,
      obj$sandwich$xy$D,
      obj$sandwich$xy$rbar,
      R,
      betas,
      ok
    )
    
    dimnames(V) <- list(obj$names, obj$names)
    attr(V, "model-based") <- R * sserr / (nobs - np + sum(!ok))
    
  } else {
    V <- R * sserr / (nobs - np + sum(!ok))
  }
  
  V

}


#' @export
deviance.online_lm <- function(obj, ...) {
  obj$qr$sserr
}


#' mimic base `print.lm` as closely as possible
#' (unofficial reference .. may go away)
#' https://github.com/wch/r-source/blob/trunk/src/library/stats/R/lm.R
#' 
#' @export
print.online_lm <- function(obj,
                            digits = max(3L, getOption("digits") - 3L),
                            ...) {
  
  cat("\nOut-of-memory Linear Model:\n",
      paste(deparse(obj$call), sep = "\n", collapse = "\n"),
      "\n\n",
      sep = "")
  
  betas <- coef(obj)
  
  if(length(betas)) {
    cat("Coefficients:\n")
    print.default(
      format(betas, digits = digits),
      print.gap = 2L,
      quote     = FALSE)
  } else {
    cat("No coefficients\n")
  }
  
  cat("\n")
  cat("Observations included: ", obj$n, "\n")

  invisible(obj)
  
}


#' mimic base `summary.lm` as closely as possible
#' (unofficial reference .. may go away)
#' https://github.com/wch/r-source/blob/trunk/src/library/stats/R/lm.R
#' 
#' @export
summary.online_lm <- function(x,
                              correlation  = FALSE,
                              symbolic.cor = FALSE,
                              ...) {
  
  lindep        <- as.logical(x$qr$lindep())
  names(lindep) <- x$names
  
  np      <- x$qr$np
  nobs    <- x$qr$nobs
  sumsqy  <- x$qr$sumsqy
  rdf     <- nobs - np
  rss_all <- x$qr$rss()
  rssr    <- if (attr(x$terms, "intercept")) rss_all[1] else sumsqy
  rssf    <- rss_all[length(rss_all)]
  resvar  <- rssf / rdf
  sigma   <- sqrt(resvar)
  sserr   <- x$qr$sserr
  beta    <- coef(x)
  cov_mat <- vcov(x)
  se      <- sqrt(diag(cov_mat))
  tval    <- beta / se
  
  mat <- cbind(
    "Estimate"   = beta,
    "Std. Error" = se,
    "t value"    = tval,
    "Pr(>|t|)"   = 2 * pt(abs(tval), rdf, lower.tail = FALSE)
  )
  
  rownames(mat) <- x$names
  
  if (np != attr(x$terms, "intercept")) {
    
    df.int  <- if (attr(x$terms, "intercept")) 1L else 0L
    
    r.squared <- 1 - deviance(x) / rssr
    adj.r.squared <- 1 - (1 - r.squared) * ((nobs - df.int) / rdf)
    
    fstatistic <- c(
      value = (rssr - rssf) / (np - df.int) / resvar,
      numdf = np - df.int,
      dendf = rdf
    )
    
  } else {
    r.squared  <- adj.rsquared <- 0
    fstatistic <- NULL
  }
  
  rval <- list(
    call          = x$call,
    terms         = x$terms,
    coefficients  = mat,
    aliased       = lindep,
    df            = c(np - sum(lindep), rdf, np),
    fstatistic    = fstatistic,
    sigma         = sigma,
    r.squared     = r.squared,
    adj.r.squared = adj.r.squared,
    cov.unscaled  = cov_mat * (nobs - np + sum(lindep)) / sserr
  )
  
  if (correlation) {
    rval$correlation <- (rval$cov.unscaled * resvar) / outer(se, se)
    dimnames(rval$correlation ) <- dimnames(rval$cov.unscaled)
    rval$symbolic.cor <- symbolic.cor
  }
  
  class(rval) <- "summary.online_lm"
  rval
}



#' mimic base `summary.lm` as closely as possible
#' (unofficial reference .. may go away)
#' https://github.com/wch/r-source/blob/trunk/src/library/stats/R/lm.R
#' 
#' @export
print.summary.online_lm <- function(x,
                                    digits = max(3L, getOption("digits") - 3L),
                                    symbolic.cor = x$symbolic.cor,
                                    signif.stars = getOption("show.signif.stars"),
                                    ...) {
  
  cat("\nOut-of-memory Linear Model:\n",
      paste(deparse(x$call), sep = "\n", collapse = "\n"),
      "\n\n", sep = "")

  printCoefmat(x$coefficients,
               digits = digits,
               signif.stars = signif.stars,
               na.print = "NA",
               ...)
  
  # if (!is.null(x$obj$sandwich)) {
  #   cat("Sandwich (model-robust) standard errors.\n")
  # }
  
  cat("\nResidual standard error:",
      format(signif(x$sigma, digits)),
      "on",
      x$df[2L],
      "degrees of freedom")
  cat("\n")
  
  # 
  # if(nzchar(mess <- naprint(x$na.action))) {
  #   cat("  (",mess, ")\n", sep = "")
  # }

  if (!is.null(x$fstatistic)) {
    cat("Multiple R-squared: ", formatC(x$r.squared, digits = digits))
    cat(",\tAdjusted R-squared: ",
        formatC(x$adj.r.squared, digits = digits),
        "\nF-statistic:",
        formatC(x$fstatistic[1L], digits = digits),
        "on",
        x$fstatistic[2L],
        "and",
        x$fstatistic[3L],
        "DF,  p-value:",
        format.pval(
          pf(x$fstatistic[1L],
             x$fstatistic[2L],
             x$fstatistic[3L],
             lower.tail = FALSE),
          digits = digits))
  }
  
  cat("\n")
    
  correl <- x$correlation
  if (!is.null(correl)) {
    p <- ncol(correl)
    if (p > 1L) {
      cat("\nCorrelation of Coefficients:\n")
      if(is.logical(symbolic.cor) && symbolic.cor) {
	      print(symnum(correl, abbr.colnames = NULL))
      } else {
        correl <- format(round(correl, 2), nsmall = 2, digits = digits)
        correl[!lower.tri(correl)] <- ""
        print(correl[-1, -p, drop=FALSE], quote = FALSE)
      }
    }
  }
    
  cat("\n")

  invisible(x)

}
