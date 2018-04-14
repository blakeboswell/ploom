
# predict.ylm <- function(object,
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
# variable.names.ylm <- function(object, full = FALSE, ...) { not possible }
#
# case.names.ylm <- function(object, full = FALSE, ...) { not possible }
#
# anova.ylm <- function(object, ...) { not possible }


#' @export
formula.ylm <- function(x, ...) {
  formula(x$terms)
}


#' @export
family.ylm <- function(object, ...) {
  gaussian() 
}


#' @export
deviance.ylm <- function(obj, ...) {
  obj$qr$rss_full
}


#' @export
AIC.ylm <- function(obj, ..., k = 2) {

  p   <- obj$qr$rank()
  rss <- obj$qr$rss_full
  n   <- obj$n - obj$zero_weights
  pw  <- obj$pweights
  
  (-(pw - n * (log(2 * pi) + 1 - log(n) + log(rss)))
    + k * (p + 1))
  
}


#' @export
coef.ylm <- function(obj, ...) {
  betas        <- coef(obj$qr)
  names(betas) <- obj$names
  betas
}


#' @export
vcov.ylm <- function(obj, ...) {
  
  ## cpp implementation / HC not supported ---------------------------
  V <- vcov(obj$qr)
  dimnames(V) <- list(obj$names, obj$names)

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

