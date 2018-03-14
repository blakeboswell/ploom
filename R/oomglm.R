#' @include oommodel.R

#' @keywords internal
init_oomglm <- init_model(model_class = c('oomglm', 'oomlm'))


#' @keywords internal
glm_transform <- function(obj, chunk) {
  
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


#' @export
update_oomglm <- update_oommodel(response_transform = glm_transform)


#' @export
oomglm <- function(formula,
                   data     = NULL,
                   family   = gaussian(),
                   weights  = NULL,
                   sandwich = FALSE) {
  
  obj <- init_oomglm(formula, family, weights, sandwich)

  if(!is.null(data)) {
    obj <- update_oomglm(obj, data)
  }
  
  obj
  
}


