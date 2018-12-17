
# mimic base `lm` as closely as possible
#

# @method summary oomlm
#' @export
summary.oomlm <- function(object,
                          correlation  = FALSE,
                          symbolic.cor = FALSE,
                          ...) {

  if(!inherits(object, "oomlm")) {
    stop("not an oomlm object.")
  }
  
  rank <- object$qr$rank()
  if(rank == 0) {
    # TODO
  }
  
  if(is.null(object$terms)) {
    # TODO
  }
  
  has_intercept  <- attr(object$terms, "intercept") > 0
  lindep         <- as.logical(object$qr$lindep())
  names(lindep)  <- object$names
  
  num_params     <- object$qr$num_params
  num_obs        <- object$qr$num_obs
  intercept_only <- rank == attr(object$terms, "intercept")
  
  sumsqy     <- object$qr$sumsqy
  rss        <- object$qr$rss()
  rss_full   <- tail(rss, 1)
  rss_red    <- if(has_intercept) head(rss, 1) else sumsqy
  res_dof    <- num_obs - rank
  res_var    <- rss_full / res_dof
  res_std    <- sqrt(res_var)
  
  beta       <- coef(object)
  cov_mat    <- vcov(object)
  se         <- sqrt(diag(cov_mat))
  tval       <- beta / se

  coef_mat <- cbind(
    "Estimate"   = beta,
    "Std. Error" = se,
    "t value"    = tval,
    "Pr(>|t|)"   = 2 * pt(abs(tval), res_dof, lower.tail = FALSE)
  )
  rownames(coef_mat) <- object$names
  
  if(!intercept_only) {
    
    df_int  <- if (has_intercept) 1L else 0L
    
    r_squared     <- 1 - rss_full / rss_red
    adj_r_squared <- 1 - (1 - r_squared) * ((num_obs - df_int) / res_dof)
    
    if(FALSE) {
      fstatistic <- c(
        value = (rss_red - rss_full) / (rank - df_int) / res_var,
        numdf = rank - df_int,
        dendf = res_dof
      )      
    } else {
      
      indices <- seq.int(has_intercept + 1, rank, by = 1)
      coefs   <- as.matrix(beta)
      
      value   <- tryCatch({
        sapply(seq_len(ncol(coefs)),
               function(x) {
                 vcov_indices <- indices + (x - 1) * rank
                 crossprod(
                   coefs[indices, x],
                   chol2inv(
                     chol(cov_mat[vcov_indices, vcov_indices])
                   ) %*% coefs[indices, x]
                 ) / (rank - df_int)
               })
      },
      error = function(e) {
        NA
      })
      
      fstatistic <- c(
        value = value,
        numdf = rank - df_int,
        dendf = res_dof
      )  
      
    }
    
  } else {
    r_squared  <- adj_rsquared <- 0
    fstatistic <- NULL
  }
  
  rval <- list(
    call          = object$call,
    terms         = object$terms,
    coefficients  = coef_mat,
    n             = num_obs,
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


#' @method print summary.oomlm
#' @export
print.summary.oomlm <- function(x,
                                digits = max(3L, getOption("digits") - 3L),
                                symbolic.cor = x$symbolic.cor,
                                signif.stars = getOption("show.signif.stars"),
                                ...) {
  
  cat("\nCall:  ",
      paste(deparse(x$call), sep = "\n", collapse = "\n"),
      "\n\n", sep = "")
  
  if(!is.null(x$se_type)) {
    cat(paste("Standard error type:", x$se_type), "\n\n")  
  }
  
  printCoefmat(x$coefficients,
               digits = digits,
               signif.stars = signif.stars,
               na.print = "NA",
               ...)
  
  cat("\nObservations included: ", x$n, "\n")
  
  cat("Residual standard error:",
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
