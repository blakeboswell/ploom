
#' Predict values using `oomlm` and `oomglm` models
#'
#' @param object An object inheriting from class `oomlm`.
#' @param new_data Observations for prediction.
#' @param std_error Indicates if the standard error of predicted means should
#'   be returned.
#' @param interval Type of interval calculation, "confidence" or "prediction.
#'   Is ignored if `std_error` is `FALSE`.
#' @param level Confidence level for interval calculation.
#' @param type The type of prediction for `oomglm` models, "response" or "link".
#' @param as_function If `TRUE`, a function requiring only `new_data` is
#'   returned for subsequent fitting.
#' @param ... Ignored.
#'
#' @examples \donttest{
#' # fit an `oomlm` model
#' chunks <- oomdata_tbl(mtcars, chunk_size = 1)
#' x  <- fit(oomlm(mpg ~ cyl + disp + hp), chunks)
#' 
#' # call `predict`
#' pred <- predict(x, mtcars)
#' sum((pred - mtcars$mpg)^2)
#' 
#' # pass TRUE for the `as_function` argument and the
#' # return value will be a prediction function with
#' # only one argument for data
#' pred_fun <- predict(x, mtcars, as_function = TRUE)
#' pred_fun(mtcars[1, ])
#' 
#' # pass TRUE for the `std_error` argument and the
#' # return value will include standard errors
#' # for the predicted means
#' pred <- predict(x, mtcars, std_error = TRUE)
#' head(pred)
#'
#' # with std_error true, we can specify that a confidence or
#' # prediction interval be returned
#' pred <- predict(x, mtcars, std_error = TRUE, interval = "confidence")
#' head(pred)
#' }
#' 
#' @seealso [oomlm()], [oomglm()] 
#' @name predict
NULL


#' @rdname predict
#' @export
predict.oomlm <- function(object,
                          new_data  = NULL,
                          std_error = FALSE,
                          interval  = NULL,
                          level     = 0.95,
                          as_function = FALSE, ...) {
  
  if(!as_function && is.null(new_data)){
    stop("`new_data` must be provided if `as_function` is FALSE")
  }
  
  fn <- predict_oomlm(object, std_error, interval, level)
  
  if(as_function) {
   return(fn) 
  }

  fn(new_data)
  
}


#' Internal. Return function that will predict and format output
#' 
#' @param object An `oomlm` model object.
#' @param std_error Indicates if the standard error of predicted means should
#'   be returned.
#' @param interval Type of interval calculation, "confidence" or "prediction.
#'   Is ignored if `std_error` is FALSE.
#' @param level Confidence level for interval calculation.
#' 
#' @keywords internal
predict_oomlm <- function(object,
                          std_error = FALSE,
                          interval  = NULL,
                          level     = 0.95) {
  
  function(new_data) {
    
    x <- unpack_oomchunk(object, new_data)
    y <- predict_oomlm_x(object, x, std_error, interval, level)
    
    if(std_error) {
      x <- tibble::tibble(
        .pred      = y$fit[, 1],
        .std_error = y$std_error
      )
      if(!is.null(interval)) {
        x[[".pred_lower"]] <- y$fit[, 2]
        x[[".pred_upper"]] <- y$fit[, 3]
      }
      return(x)
    }
    
    tibble::tibble(.pred = drop(y))
    
  }
   
}


#' Internal. Perform `oomlm` prediction
#' 
#' @param object An `oomlm` model object.
#' @param chunk The `list` of artifacts returned by `unpack_oomchunk()`.
#' @param std_error Indicates if the standard error of predicted means should
#'   be returned.
#' @param interval Type of interval calculation, "confidence" or "prediction.
#'   Is ignored if `std_error` is FALSE.
#' @param level Confidence level for interval calculation.
#' 
#' @keywords internal
predict_oomlm_x <- function(object, chunk,
                            std_error = FALSE,
                            interval  = NULL,
                            level     = 0.95) {

  X   <- chunk$data
  fit <- X %*% coef(object)
  
  if(std_error) {
    
    dispersion <- dispersion_oomlm(object)
    res_scale  <- as.vector(sqrt(dispersion))

    # rss    <- object$qr$rss_full
    dof    <- object$df.residual
    vcov_y <- vcov(object)
    # res_scale <- rss / dof
    
    var_y <- apply(X, 1, function(x){
      tcrossprod(crossprod(x, vcov_y), x)
    })
    
    if(!is.null(interval)) {
      predi <- res_scale * (interval %in% "prediction")
      intv  <- sqrt(predi + var_y)
      tval  <- qt((1 - level)/2, dof)
      fit   <- cbind(fit, fit + intv * tval, fit - intv * tval)
      colnames(fit) <- c(".pred", ".pred_lower", ".pred_upper")
    }
    
    return(list(fit = fit, std_error = sqrt(var_y)))
    
  }
  
  fit
  
}


#' @rdname predict
#' @export
predict.oomglm <- function(object,
                           new_data    = NULL,
                           type        = "response",
                           std_error   = FALSE,
                           as_function = FALSE, ...) {
  
  if(!as_function && is.null(new_data)){
    stop("`new_data` must be provided if `as_function` is FALSE")
  }
  
  fn <- predict_oomglm(object, type, std_error)
  
  if(as_function) {
    return(fn)
  }
  
  fn(new_data)
  
}


#' Internal. Return function that will predict and format output
#' 
#' @param object An `oomglm` model object.
#' @param type The type of prediction for `oomglm` models, "response" or "link".
#' @param std_error Indicates if the standard error of predicted means should
#'   be returned.
#'  
#' @keywords internal
predict_oomglm <- function(object,
                           type      = "response",
                           std_error = FALSE) {
  function(new_data) {
    
    x <- unpack_oomchunk(object, new_data)
    y <- predict_oomglm_x(object, x, type = type, std_error = std_error)
    
    if(std_error) {
      return(tibble::tibble(
        .pred      = drop(y$fit),
        .std_error = drop(y$std_error)
      ))
    }

    tibble::tibble(.pred = drop(y))
    
  }
}


#' Internal. Perform `oomglm` prediction
#' 
#' @param object An `oomglm` model object.
#' @param chunk The `list` of artifacts returned by `unpack_oomchunk()`.
#' @param type The type of prediction for `oomglm` models, "response" or "link".
#' @param std_error Indicates if the standard error of predicted means should
#'   be returned.
#'   
#' @keywords internal
predict_oomglm_x <- function(object, chunk,
                             type = "response",
                             std_error = FALSE) {
  
  if(!std_error) {
    
    pred <- predict_oomlm_x(object, chunk)
    
    switch(type,
           response = {pred <- family(object)$linkinv(pred)},
           link = )
    
    pred
    
  } else {

    fam  <- family(object)
    pred <- predict_oomlm_x(object, chunk, std_error = TRUE)
    fit  <- pred$fit
    se   <- pred$std_error
    
    switch(type,
           response = {
             se  <- se * abs(fam$mu.eta(fit))
             fit <- fam$linkinv(fit)
           },
           link = )
    
    list(fit = fit, std_error = se)
    
  }

}
