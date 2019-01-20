

##
## based on code by Christophe Dutang <christophe.dutang@ensimag.fr>
##

#' @method predict oomlm
#' @export
predict.oomlm <-function(object,
                         newdata = NULL,
                         se_fit  = FALSE,
                         make_function = FALSE,
                         ...) {
  if(!make_function && is.null(newdata)){
    stop("Must provide newdata")
  }
  
  predict.oomglm(object,
                 newdata = newdata,
                 type    = "link",
                 se_fit  = se_fit,
                 make_function = make_function)
}


#' @method predict oomglm
#' @export
predict.oomglm <- function(object,
                           newdata,
                           type   = c("link", "response"),
                           se_fit = FALSE,
                           make_function = FALSE,
                           ...) {
  
  type      <- match.arg(type)
  intercept <- attr(object$terms,"intercept") != 0
  
  if(intercept && make_function) {
    
    switch(type,
      link =
      {
        fit <- function(x) {
          cbind(1, x) %*% coef(object)
        }
        se  <- function(x) {
          cbind(1, x) %*% vcov(object) %*% t(cbind(1, x))
        }
      },
      response =
      {
        fit <- function(x) {
          family(object)$linkinv(cbind(1,x) %*% coef(object))
        }
        se <- function(x) {
          temp <- cbind(1,x) %*% vcov(object) %*% t(cbind(1,x))
          temp %*% (family(object)$mu.eta(cbind(1, x) %*% coef(object)))^2
        }
      })
    
  } else {
    
    switch(type,
      link =
      {
        fit <- function(x) {
          x %*% coef(object)
        }
        se  <- function(x){ 
          x %*% vcov(object) %*% t(x)
        }
      },
      response =
      {
        fit <- function(x) {
          family(object)$linkinv(x %*% coef(object))
        }
        se  <- function(x) {
          x %*% vcov(object) %*% t(x) %*% (family(object)$mu.eta(x %*%coef(object)))^2
        }
      })
    
  }
  
  if (make_function) {
    if (se_fit) {
      return(list(fit = fit, se = se))
    } else {
      return(fit)
    }
  }
  
  newmf <- model.frame(object$terms, newdata)
  newmm <- model.matrix(object$terms, newmf)
  
  if (!se_fit) {
    pred <- fit(newmm)
  } else {
    fit    <- fit(newmm)
    se_fit <- se_fit(newmm)
    pred   <- list(fit = fit, se = se)
  }
  
  pred
  
}
