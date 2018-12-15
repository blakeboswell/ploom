

#' @export
#' @method formula oomlm
formula.oomlm <- function(x, ...) {
  formula(x$terms)
}


#' @export
#' @method family oomlm
family.oomlm <- function(object, ...) {
  gaussian()
}


#' @export
#' @method deviance oomlm
deviance.oomlm <- function(object, ...) {
  object$qr$rss_full
}


#' @export
#' @method AIC oomlm
AIC.oomlm <- function(object, ..., k = 2) {

  p   <- object$qr$rank()
  rss <- object$qr$rss_full
  n   <- object$qr$num_obs
  pw  <- object$qr$pweights
  
  (-(pw - n * (log(2 * pi) + 1 - log(n) + log(rss)))
    + k * (p + 1))
  
}


#' @export
#' @method coef oomlm
coef.oomlm <- function(object, ...) {
  betas        <- coef(object$qr)
  names(betas) <- object$names
  betas
}


#' @export
#' @method vcov oomlm
vcov.oomlm <- function(object, ...) {
  
  ## cpp implementation / HC not supported ---------------------------
  V <- vcov(object$qr)
  dimnames(V) <- list(object$names, object$names)

  V
  
  # biglm implementation --------------------------------------------
  
  # nobs  <- obj$qr$num_obs
  # np    <- obj$qr$num_params
  # sserr <- obj$qr$rss_full
  # ok    <- !obj$qr$lindep()
  # 
  # R  <- rvcov_biglm(
  #   np,
  #   obj$qr$D,
  #   obj$qr$rbar,
  #   ok
  # )
  # 
  # dimnames(R) <- list(obj$names, obj$names)
  # 
  # if(!is.null(obj$sandwich)) {
  #   
  #   betas <- coef(obj$qr)
  #   
  #   V <- sandwich_rcov_biglm(
  #     np,
  #     obj$sandwich$xy$D,
  #     obj$sandwich$xy$rbar,
  #     R,
  #     betas,
  #     ok
  #   )
  #   
  #   dimnames(V) <- list(obj$names, obj$names)
  #   attr(V, "model-based") <- R * sserr / (nobs - np + sum(!ok))
  #   
  # } else {
  #   V <- R * sserr / (nobs - np + sum(!ok))
  # }
  # 
  # V
  
}

