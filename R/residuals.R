
#' calculate residuals for oomglm
#'
#' @param object `oomlm` or `oomglm` model
#' @param data data to calculate residuals
#' @param type residual calculation method
#' @param as_function if TRUE a function with only a `data` argument is returned
#'   for subsequent residual calculations
#' @param ... ignored  
#' 
#' @name residuals
NULL


#' @rdname residuals
#' @method residuals oomlm
#' @export
resid.oomlm <- function(object, data,
                        as_function = FALSE, ...) {
  residuals.oomlm(object, data, as_function = FALSE, ...)
}

#' @rdname residuals
#' @method residuals oomlm
#' @export
residuals.oomlm <- function(object, data,
                            as_function = FALSE, ...) {
  
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
    chunk <- unpack_oomchunk(object, data)
    r <- residuals_oomlm_x(object, chunk)
    tibble::tibble(.resid = drop(r))
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
  y - yhat
}


#' @rdname residuals
#' @method residuals oomglm
#' @export
resid.oomglm <- function(object, data,
                         type = c("deviance"
                                  , "pearson"
                                  , "response"
                                  , "working"),
                         as_function = FALSE, ...) {
  
  residuals.oomglm(object, data, type, as_function)
  
}



#' @rdname residuals
#' @method residuals oomglm
#' @export
residuals.oomglm <- function(object, data,
                             type = c("deviance"
                                      , "pearson"
                                      , "response"
                                      , "working"),
                         as_function = FALSE, ...) {
  
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
    chunk <- unpack_oomchunk(object, data)
    r <- residuals_oomglm_x(object, chunk, type)
    tibble::tibble(.resid = drop(r))
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
  
  res
  
}
