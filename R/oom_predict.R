
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

  
  link_pred <- function(data) {
    
    x  <- model_frame(object$terms, data)
    x  <- model_matrix(object$terms, x)
    
    if(se_fit) {
      return(list(
        fit = x %*% coef(object),
        se  = x %*% vcov(object) %*% t(x)))
    }
    
    x %*% coef(object)
    
  }
  
  if(type == "response") {
    
    fam      <- family(object)
    linkinv  <- fam$linkinv
    mu_eta   <- fam$mu.eta
    
    resp_pred <- function(data) {
      
      pred <- link_pred(data)
      
      if(se_fit) {
        y  <- pred$fit
        return(list(
          fit = y,
          se  = pred$se %*% mu_eta(y)^2))
      }
      
      linkinv(pred)
      
    }
    
    fitfxn <- resp_pred
    
  } else {
    fitfxn <- link_pred
  }
  
  if(as_function) {
    return(fitfxn)
  }
  
  fitfxn(data)
  
}

