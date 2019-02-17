#' Calculate residuals for `oomlm` and `oomglm` models
#'
#' Returns the difference between actual and predicted values.
#' Since `oomlm` models do not store data while fitting, `data`
#' must be supplied to calculate residuals.
#'
#' @param object An object inheriting from class `oomlm`
#' @param data Observations for residual calculation.
#' @param type Residual calculation method for `oomglm` models.
#' @param as_function If `TRUE`, a function requiring only `data` is
#'   returned for subsequent residual calculations.
#' @param ... Ignored.
#' 
#' @seealso [oomlm()], [oomglm()] 
#' @name residuals
NULL


#' @rdname residuals
#' @export
residuals.oomlm <- function(object,
                            data        = NULL,
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


#' @rdname residuals
#' @export
residuals.oomglm <- function(object,
                             data = NULL,
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


#' Internal. Wrapper for unpacking data and calculating residuals
#' 
#' @param object An `oomlm` model.
#' 
#' @keywords internal
residuals_oomlm <- function(object) {
  function(data) {
    chunk <- unpack_oomchunk(object, data)
    r <- residuals_oomlm_x(object, chunk)
    tibble::tibble(.resid = drop(r))
  }
}


#' Internal.  Calculate residuals of `oomlm` model
#' 
#' @param object An `oomlm` model.
#' @param x The `list` of artifacts returned by `unpack_oomchunk()`.
#' 
#' @keywords internal
residuals_oomlm_x <- function(object, x) {
  y    <- x$response - x$offset
  pred <- x$data %*% coef(object)
  y - pred
}


#' Internal. Wrapper for unpacking data and calculating residuals
#'
#' @param object An `oomglm` model.
#' @param `type` Residual calculation method.
#' 
#' @keywords internal
residuals_oomglm <- function(object,
                             type = c("deviance"
                                      , "pearson"
                                      , "response"
                                      , "working")) {
  
  type <- match.arg(type)
  
  function(data) {
    X     <- unpack_oomchunk(object, data)
    pred  <- predict_oomglm_x(object, X, type = "response")
    u     <- residuals_oomglm_x(object, X, pred, type)
    tibble::tibble(.resid = drop(u))
  }
  
}


#' Internal.  Calculate residuals of `oomglm` model.
#' 
#' @param object An `oomglm` model.
#' @param x The `list` of artifacts returned by `unpack_oomchunk()`.
#' @param pred predicted response
#' @param `type` Residual calculation method.
#' 
#' @keywords internal
residuals_oomglm_x <- function(object, x, pred, type) {

  fam  <- object$family
  wts  <- x$weights
  y    <- x$response - x$offset
  
  res <- switch(
    type,
    deviance = if(object$df.residual > 0) {
      dres <- sqrt(pmax(fam$dev.resids(y, pred, wts), 0))
      ifelse(y > pred, dres, -dres)
    } else {
      rep(0.0, length(pred))
    },
    pearson  = {
      (y - pred) * sqrt(wts) / sqrt(fam$variance(pred))
    },
    response = y - pred
  )
  
  res
  
}


