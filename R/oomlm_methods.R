
#' @export
deviance.oomlm <- function(obj, ...) {
  obj$qr$rss_full
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
