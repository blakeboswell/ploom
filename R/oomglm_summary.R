

#' @export
summary.oomglm <- function(x,
                           correlation  = FALSE,
                           symbolic.cor = FALSE,
                           ...) {

    rval <- summary.oomlm(x, correlation, symbolic.cor, ...)
    
    rval$family      <- x$family  
    rval$deviance    <- x$iwls$deviance
    rval$aic         <- AIC(x)
    rval$df.residual <- x$df.residual
    rval$df.null     <- x$df.null
    rval$iter        <- x$iter
    
    class(rval) <- "summary.oomglm"
    
    rval
}


#' @export
print.summary.oomglm <- function(x,
                                 digits = max(3L, getOption("digits") - 3L),
                                 symbolic.cor = x$symbolic.cor,
                                 signif.stars = getOption("show.signif.stars"),
                                 ...) {
  
  cat("\nOut-of-memory Generalized Linear Model:\n",
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
  
  cat("\n")
  
  # cat("\n(Dispersion parameter for ", x$family$family,
  #     " family taken to be ", format(x$dispersion), ")\n\n",

  cat(
      apply(cbind(paste(format(c("Null","Residual"), justify="right"),
                        "deviance:"),
                  format(unlist(x[c("null.deviance","deviance")]),
                         digits = max(5L, digits + 1L)), " on",
                  format(unlist(x[c("df.null","df.residual")])),
                  " degrees of freedom\n"),
            1L, paste, collapse = " "), sep = "")
  
  # 
  # if(nzchar(mess <- naprint(x$na.action))) {
  #   cat("  (",mess, ")\n", sep = "")
  # }
  
  cat("AIC: ", format(x$aic, digits = max(4L, digits + 1L)),"\n\n",
      "Number of Fisher Scoring iterations: ", x$iter,
      "\n", sep = "")
  
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