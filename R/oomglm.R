

#' @export
oomglm <- function(formula,
                   data     = NULL,
                   family   = gaussian(),
                   weights  = NULL,
                   sandwich = FALSE) {
  
  obj <- model_init(formula, family, weights, sandwich)

  if(!is.null(data)) {
    obj <- model_update(data, obj, glm_func)
  }
  
  obj
  
}


#' @export
update_oomglm <- function(obj, data) {
  model_update(data, obj, glm_func)
}


#' @keywords internal
glm_func <- function(chunk, obj) {
  
  mm     <- chunk$data
  y      <- chunk$response
  w      <- chunk$weights
  offset <- chunk$offset
  
  family <- obj$family
  beta   <- obj$fitstats$beta
  
  if(is.null(beta)) {
    eta <- rep(0, nrow(mm)) + offset
  } else {
    eta <- mm %*% beta + offset
  }
  
  mu  <- family$linkinv(eta)
  dmu <- family$mu.eta(eta)
  z   <- eta + (y - mu) / dmu
  w   <- w * dmu * dmu / family$variance(mu)
  
  list(
    z = z,
    w = w
  )
  
}