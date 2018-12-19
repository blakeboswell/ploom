
#' @method formula oomlm
#' @export
formula.oomlm <- function(x, ...) {
  formula(x$terms)
}


#' @method family oomlm
#' @export
family.oomlm <- function(object, ...) {
  gaussian()
}


#' @method deviance oomlm
#' @export
deviance.oomlm <- function(object, ...) {
  object$qr$rss_full
}


#' @method AIC oomlm
#' @export
AIC.oomlm <- function(object, ..., k = 2) {

  p   <- object$qr$rank()
  rss <- object$qr$rss_full
  n   <- object$qr$num_obs
  pw  <- object$qr$pweights
  
  (-(pw - n * (log(2 * pi) + 1 - log(n) + log(rss)))
    + k * (p + 1))
  
}


#' @method coef oomlm
#' @export
coef.oomlm <- function(object, ...) {
  betas        <- coef(object$qr)
  names(betas) <- object$names
  betas
}


#' @method confint oomlm
#' @export
confint.oomlm <- function(object, parm, level = 0.95, ...) {
  confint.lm(object, parm, level = 0.95, ...)  
}


#' @method vcov oomlm
#' @export
vcov.oomlm <- function(object, ...) {
  
  ## cpp implementation / HC not supported ---------------------------
  V <- vcov(object$qr)
  dimnames(V) <- list(object$names, object$names)

  V
  
}


#' @method vcov oomlm_robust
#' @export
vcov.oomlm_robust <- function(object, ...) {
  
  if(object$se_type == "classical") {
    return(vcov.oomlm(object, ...))
  }
  
  # biglm implementation --------------------------------------------
  
  nobs  <- object$qr$num_obs
  np    <- object$qr$num_params
  sserr <- object$qr$rss_full
  ok    <- !object$qr$lindep()
  
  R <- object$qr$sdm_inv()
  dimnames(R) <- list(object$names, object$names)

  betas <- coef(object$qr)
  
  V <- sandwich_rcov_biglm(
    np,
    object$sandwich$xy$D,
    object$sandwich$xy$rbar,
    R,
    betas,
    ok
  )
  
  dimnames(V) <- list(object$names, object$names)
  
  if(object$se_type %in% c("HC1", "stata")) {
    V * nobs / (nobs - sum(ok))
  } else {
    # HCO
    V
  }

  
}


#' @method vcov oomglm_robust
#' @export
vcov.oomglm_robust <- function(object, ...) {
  vcov.oomlm_robust(object, ...)
}
