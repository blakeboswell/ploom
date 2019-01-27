
#' calculate residuals for oomglm
#'
#' @param object `oomlm` or `oomglm` model
#' @param data data to calculate residuals
#' @param type residual calculation method
#' @param as_function if TRUE a function with only a `data` argument is returned
#'   for subsequent residual calculations
#' @name resid
NULL

resid     <- function (x, ...) { UseMethod("resid", x) }
residuals <- function (x, ...) { UseMethod("resid", x) }

#' @rdname resid
#' @export
resid.oomlm <- function(object, data, as_function = FALSE, ...) {
  
  u <- residuals_oomlm(object, data, as_function)
  
  if(as_function) {
    return(u)
  }
  
  tibble::tibble(.resid = u)
  
}

#' @rdname resid
#' @export
resid.oomglm <- function(object,
                         data,
                         type = c("deviance"
                                  , "pearson"
                                  , "response"
                                  , "working"),
                         as_function = FALSE,
                         ...) {
  
  u <- residuals_oomglm(object, data, type, as_function)
  
  if(as_function) {
    return(u)
  }
  
  tibble::tibble(.resid = u)
  
}



#' @rdname resid
#' @keywords internal
residuals_oomlm <- function(object, data, as_function = FALSE) {
  
  if(!as_function && is.null(data)){
    stop("`data` must be provided if `as_function` is FALSE")
  }
  
  fn <- function(data) {
    x    <- unpack_oomchunk(object, data)
    y    <- x$response - x$offset
    yhat <- x$data %*% coef(object)
    
    drop(y - yhat)
  }
  
  if(as_function) {
    return(fn)
  }
  
  fn(data)
  
}


#' @rdname resid
#' @keywords internal
residuals_oomglm <- function(object,
                             data,
                             type = c("deviance"
                                      , "pearson"
                                      , "response"
                                      , "working"),
                             as_function = FALSE) {
  
  if(!as_function && is.null(data)){
    stop("`data` must be provided if `as_function` is FALSE")
  }
  
  type <- match.arg(type)
  
  fn <- function(data) {
    
    fam  <- object$family
    
    x    <- unpack_oomchunk(object, data)
    xadj <- glm_adjust(object, x)
    wts  <- xadj$w
    eta  <- xadj$z
    y    <- x$response
    yhat <- x$data %*% coef(object)
    r    <- y - yhat
    
    switch(
      type,
      deviance=,pearson=,response=
        if(is.null(y)) {
          y <- yhat + r * fam$mu.eta(eta)
        })
    
    res <- switch(
      type,
      deviance = if(object$df.residual > 0) {
        d.res <- sqrt(pmax(fam$dev.resids(y, yhat, wts), 0))
        ifelse(y > yhat, d.res, -d.res)
      } else {
        rep(0, length(yhat))
      },
      pearson  = (y - yhat) * sqrt(wts) / sqrt(fam$variance(yhat)),
      working  = r,
      response = y - yhat
    )
    
    res
    
  }
  
  if(as_function) {
    return(fn)  
  }
  
  fn(data)
  
}