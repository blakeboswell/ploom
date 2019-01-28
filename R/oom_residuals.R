
#' calculate residuals for oomglm
#'
#' @param object `oomlm` or `oomglm` model
#' @param data data to calculate residuals
#' @param type residual calculation method
#' @param as_function if TRUE a function with only a `data` argument is returned
#'   for subsequent residual calculations
#' @name resid
NULL


#' @rdname resid
resid     <- function (x, ...) { UseMethod("resid", x) }

#' @rdname resid
residuals <- function (x, ...) { UseMethod("resid", x) }


#' @rdname resid
#' @export
resid.oomlm <- function(object, data, as_function = FALSE, ...) {
  
  if(!as_function && is.null(data)){
    stop("`data` must be provided if `as_function` is FALSE")
  }
  
  fn <- residuals_oomlm(object)
  if(as_function) {
    return(fn)
  }
  
  fn(data)
  
}


#' internal wrapper for unpacking data and calculating residuals
#' 
#' @param object `oomlm` object
#' 
#' @keywords internal
residuals_oomlm <- function(object) {
  function(data) {
    x <- unpack_oomchunk(object, data)
    residuals_oomlm_x(object, x)
  }
}


#' internal `oomlm` residual calculation
#' 
#' @param x `list` of artifacts returned by `unpack_oomchunk()`
#' 
#' @keywords internal
residuals_oomlm_x <- function(object, x) {
  y    <- x$response - x$offset
  yhat <- x$data %*% coef(object)
  r    <- drop(y - yhat)
  tibble::tibble(.resid = r)
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
  
  if(!as_function && is.null(data)){
    stop("`data` must be provided if `as_function` is FALSE")
  }
  
  fn <- residuals_oomglm(object, type)
  if(as_function) {
    return(fn)
  }
  
  fn(data)
  
}


#' internal wrapper for unpacking data and calculating residuals
#' 
#' @param object `oomglm` object
#' @param type residual calculation method
#' 
#' @keywords internal
residuals_oomglm <- function(object,
                             type = c("deviance"
                                      , "pearson"
                                      , "response"
                                      , "working")) {
  
  type <- match.arg(type)
  
  function(data) {
    x    <- unpack_oomchunk(object, data)
    residuals_oomglm_x(object, x, type)
  }
  
}


#' internal `oomglm` residual calculation
#' 
#' @param x `list` of artifacts returned by `unpack_oomchunk()`
#' 
#' @keywords internal
residuals_oomglm_x <- function(object, x, type) {

  fam  <- object$family
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
  
  tibble::tibble(.resid = drop(res))
  
}
