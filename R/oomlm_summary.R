# mimic base `lm` as closely as possible
# (unofficial reference ..)
# https://github.com/wch/r-source/blob/trunk/src/library/stats/R/lm.R


#' @export
summary.oomlm <- function(x,
                          correlation  = FALSE,
                          symbolic.cor = FALSE,
                          ...) {

  if(!inherits(x, "oomlm")) {
    stop("not an oolm object.")
  }
  
  rank <- x$qr$rank()
  if(rank == 0) {
    # TODO
  }
  
  if(is.null(x$terms)) {
    # TODO
  }
  
  has_intercept  <- attr(x$terms, "intercept") > 0
  lindep         <- as.logical(x$qr$lindep())
  names(lindep)  <- x$names
  
  num_params     <- x$qr$num_params
  num_obs        <- x$qr$num_obs
  intercept_only <- rank == attr(x$terms, "intercept")
  
  sumsqy     <- x$qr$sumsqy
  rss        <- x$qr$rss()
  rss_full   <- rss[length(rss)]
  rss_red    <- if(has_intercept) rss[1] else sumsqy
  res_dof    <- num_obs - rank
  res_var    <- rss_full / res_dof
  res_std    <- sqrt(res_var)
  
  beta       <- coef(x)
  cov_mat    <- vcov(x)
  se         <- sqrt(diag(cov_mat))
  tval       <- beta / se

  coef_mat <- cbind(
    "Estimate"   = beta,
    "Std. Error" = se,
    "t value"    = tval,
    "Pr(>|t|)"   = 2 * pt(abs(tval), res_dof, lower.tail = FALSE)
  )
  rownames(coef_mat) <- x$names
  
  if(!intercept_only) {
    
    df_int  <- if (has_intercept) 1L else 0L
    
    r_squared     <- 1 - rss_full / rss_red
    adj_r_squared <- 1 - (1 - r_squared) * ((num_obs - df_int) / res_dof)
    
    fstatistic <- c(
      value = (rss_red - rss_full) / (rank - df_int) / res_var,
      numdf = rank - df_int,
      dendf = res_dof
    )
    
  } else {
    r_squared  <- adj_rsquared <- 0
    fstatistic <- NULL
  }
  
  rval <- list(
    call          = x$call,
    terms         = x$terms,
    coefficients  = coef_mat,
    aliased       = lindep,
    df            = c(rank, res_dof, num_params),
    fstatistic    = fstatistic,
    sigma         = res_std,
    r.squared     = r_squared,
    adj.r.squared = adj_r_squared,
    cov.unscaled  = cov_mat * (num_obs - rank) / rss_full
  )
  
  if (correlation) {
    corr_mat           <- (rval$cov.unscaled * res_var) / outer(se, se)
    dimnames(corr_mat) <- dimnames(rval$cov.unscaled)
    rval$correlation   <- corr_mat
    rval$symbolic.cor  <- symbolic.cor
  }
  
  class(rval) <- "summary.oomlm"
  rval
  
}


#' temporary hack.. should use generic signature
#' @keywords internal
call_format <- function(x) {
  
  f <- paste(", formula =", deparse(formula(x)))
  w <- s <- ""
  
  if(!is.null(x$weights)) {
    w <- paste(", weights =", deparse(x$weights))
  }
  
  if(!is.null(x$sandwich)) {
    s <- ", sandwich = TRUE"
  }
  
  call_format <- paste0('oomlm(`data`', f, w, s, ')')
  
}


#' @export
print.summary.oomlm <- function(x,
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
