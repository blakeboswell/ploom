
#' @method summary oomglm
#' @export
summary.oomglm <- function(object,
                           correlation  = FALSE,
                           symbolic.cor = FALSE, ...) {

    rval <- summary.oomlm(object, correlation, symbolic.cor, ...)
    
    rval$family      <- object$family  
    rval$deviance    <- object$qr$rss_full
    rval$aic         <- AIC(object)
    rval$df.residual <- object$df.residual
    rval$df.null     <- object$df.null
    rval$iter        <- object$iter
    
    class(rval) <- "summary.oomglm"
    
    rval
}


#' @method print summary.oomglm
#' @export
print.summary.oomglm <- function(x,
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
  cat(
    apply(cbind(paste(format(c("Residual"), justify="right"),
                      "deviance:"),
                format(unlist(x[c("deviance")]),
                       digits = max(5L, digits + 1L)), " on",
                format(unlist(x[c("df.residual")])),
                " degrees of freedom\n"),
          1L, paste, collapse = " "), sep = "")
  

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
