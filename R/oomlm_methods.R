
# predict.oolm <- function(object,
#                          newdata,
#                          se.fit = FALSE,
#                          scale = NULL,
#                          df = Inf, 
#                          interval = c("none", "confidence", "prediction"),
#                          level = .95,
#                          type = c("response", "terms"),
#                          terms = NULL,
#                          na.action = na.pass,
#                          pred.var = res.var/weights,
#                          weights = 1,
#                          ...) { next in line }
#
# variable.names.oomlm <- function(object, full = FALSE, ...) { not possible }
#
# case.names.oomlm <- function(object, full = FALSE, ...) { not possible }
#
# anova.oomlm <- function(object, ...) { not possible }


#' @export
formula.oomlm <- function(x, ...) {
  formula(x$terms)
}


#' @export
family.oomlm <- function(object, ...) {
  gaussian() 
}


#' @export
deviance.oomlm <- function(obj, ...) {
  obj$qr$rss_full
}


#' @export
AIC.oomlm <- function(object, ..., k = 2) {
  deviance(object) + k * (object$n - object$df.resid)
}


#' @export
coef.oomlm <- function(obj, ...) {
  betas        <- coef(obj$qr)
  names(betas) <- obj$names
  betas
}


#' @export
vcov.oomlm <- function(obj, ...) {
  
  ## cpp implementation / HC not supported ---------------------------
  # V <- vcov(obj$qr)
  # dimnames(V) <- list(obj$names, obj$names)
  # 
  # V
  
  # biglm implementation --------------------------------------------
  
  nobs  <- obj$qr$num_obs
  np    <- obj$qr$num_params
  sserr <- obj$qr$rss_full
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
print.oomlm <- function(obj,
                        digits = max(3L, getOption("digits") - 3L),
                        ...) {
  
  cat("\nOut-of-memory Linear Model:\n",
      paste(deparse(obj$call), sep = "\n", collapse = "\n"),
      "\n\n",
      sep = "")
  
  beta <- coef(obj)
  
  if(length(beta)) {
    cat("Coefficients:\n")
    print.default(
      format(beta, digits = digits),
      print.gap = 2L,
      quote     = FALSE)
  } else {
    cat("No coefficients\n")
  }
  
  cat("\n")
  cat("Observations included: ", obj$n, "\n")
  
  invisible(obj)
  
}
