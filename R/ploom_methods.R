
# predict.oomlm <- function(object,
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


#' @method vcov oomlm
#' @export
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

