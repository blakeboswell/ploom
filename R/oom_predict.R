
#' @method predict oomlm
#' @export
predict.oomlm <-function(object,
                         data    = NULL,
                         se_fit  = FALSE,
                         as_function = FALSE,
                         ...) {
  
  if(!as_function && is.null(data)){
    stop("`data` must be provided if `as_function` is FALSE")
  }
  
  predict.oomglm(object,
                 data    = data,
                 type    = "link",
                 se_fit  = se_fit,
                 as_function = as_function)
}


#' @method predict oomglm
#' @export
predict.oomglm <- function(object, 
                           data,
                           type   = "link",
                           se_fit = FALSE,
                           as_function = FALSE) {

  link_fit <- function(x) {
    x %*% coef(object)
  }
  
  link_se <- function(x) {
    x %*% vcov(object) %*% t(x)
  }
  
  link_pred <- function(x) {
    xf  <- model_frame(object$terms, data)
    xm  <- model_matrix(object$terms, xf)
    if(se_fit) {
      return(list(
        fit = link_fit(xm),
        se  = link_se(xm)))
    }
    link_fit(xm)
  }
  
  fitfxn <- link_pred

  if(type == "response") {
    
    fam      <- family(object)
    linkinv  <- fam$linkinv
    mu_eta   <- fam$mu.eta
    
    resp_pred <- function(x) {
      pred <- link_pred(x)
      y    <- linkinv(pred$fit)
      se   <- pred$se
      if(se_fit) {
        return(list(
          fit = y,
          se  = se %*% mu_eta(y)^2))
      }
      y
    }
    
    fitfxn <- resp_pred
    
  }
  
  if(as_function) {
    return(fitfxn)
  }
  
  fitfxn(data)
  
}

