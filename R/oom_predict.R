
#' Predict values using `oomlm()` and `oomglm()` models
#'
#' @param object object inheriting from class `oomlm`
#' @param new_data observations for prediction
#' @param srd_error indicates if the standard error of predicted means should
#'   be returned
#' @param interval type of interval calculation
#' @param level tolerance/confidence level for interval calculation
#' @param type the type of prediction
#' @param as_function if TRUE a function with only a `data` argument is returned
#'   for subsequent fitting
#' @param ... ignored
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
#' rss      <- 0
#' while(!is.null(chunk <- chunks())) {
#'  rss <- rss + (pred_fun(chunk) - chunk[, "mpg"])^2
#' }
#' rss
#' 
#' # pass TRUE for the `se_fit` argument and the
#' # return value will include standard errors
#' # for the predicted means
#' pred <- predict(x, mtcars, se_fit = TRUE)
#' names(pred)
#' head(pred$se)
#'
#' }
#' @name predict
NULL


#' @rdname predict
#' @export
predict.oomlm <- function(object,
                          new_data  = NULL,
                          std_error = FALSE,
                          interval  = NULL,
                          level     = 0.95,
                          as_function = FALSE,
                          ...) {
  
  if(!as_function && is.null(new_data)){
    stop("`new_data` must be provided if `as_function` is FALSE")
  }
  
  fn <- predict_oomlm(object, new_data, std_error, interval, level)
  
  if(as_function) {
   return(fn) 
  }

  fn(new_data)
  
}


#' @rdname predict
#' @keywords internal
predict_oomlm <- function(object, new_data,
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


#' @rdname predict
#' @keywords internal
predict_oomlm_x <- function(object,
                            x,
                            std_error = FALSE,
                            interval  = NULL,
                            level     = 0.95) {

  return_dof <- all(class(object) %in% c("oomlm"))
  fit <- x$data %*% coef(object)
  
  if(std_error) {
    
    rss    <- object$qr$rss_full
    dof    <- object$df.residual
    vcov_y <- vcov(object)
    res_scale <- rss / dof
    
    var_y <- apply(x$data, 1, function(x){
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
predict.oomglm <- function(object, new_data,
                           type = "response",
                           std_error   = FALSE,
                           as_function = FALSE,
                           ...) {
  
  if(!as_function && is.null(new_data)){
    stop("`new_data` must be provided if `as_function` is FALSE")
  }
  
  fn <- predict_oomglm(object, type, std_error)
  
  if(as_function) {
    return(fn)
  }
  
  fn(new_data)
  
}


#' @rdname predict
#' @keywords internal
predict_oomglm <- function(object,
                           type = "response",
                           std_error = FALSE) {
  function(new_data) {
    
    x <- unpack_oomchunk(object, new_data)
    y <- predict_oomglm_x(object, x, type = type, std_error = std_error)
    
    if(std_error) {
      return(tibble::tibble(
        .pred      = y$fit[, 1],
        .std_error = y$std_error
      ))
    }

    tibble::tibble(.pred = drop(y))
    
  }
}


#' @rdname predict
#' @keywords internal
predict_oomglm_x <- function(object, data, type = "response", std_error = FALSE) {
  
  if(type == "link") {
    
    y <- predict_oomlm_x(object, data, std_error)
    
    if(std_error) {
      return(list(fit = y$fit, std_error = y$std_error))
    }
    
    y
    
  } else {
    
    fam      <- family(object)
    linkinv  <- fam$linkinv
    mu_eta   <- fam$mu.eta
      
    z <- predict_oomlm_x(object, data, std_error)
      
    if(std_error) {
      std_error <- z$std_error * abs(mu_eta(z$fit))
      y  <- linkinv(z$fit)
      return(list(fit = y, std_error = std_error))
    } else {
      y <- linkinv(z)
    }
    
    y
      
  }
  
}

