#' Predict values using `oomlm()` and `oomglm()` models
#'
#' @param object object inheriting from class `oomlm`
#' @param data observations for prediction
#' @param se_fit indicates if the standard error of predicted means should
#'   be returned
#' @param interval type of interval calculation
#' @param type the type of prediction, one of "reponse" or "link"
#' @param as_function if TRUE a function with only a `data` argument is returned
#'   for subsequent fitting
#' @param ... ignored
#'
#' @examples \donttest{
#' # fit an `oomlm` model
#' chunks <- oomdata_tbl(mtcars, chunk_size = 1)
#' x  <- oomlm(mpg ~ cyl + disp + hp, chunks)
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
                          data     = NULL,
                          se_fit   = FALSE,
                          interval = c("none", "confidence", "prediction"),
                          as_function = FALSE,
                          ...) {
  
  if(!as_function && is.null(data)){
    stop("`data` must be provided if `as_function` is FALSE")
  }
  
  fitfxn <- function(data) {
    
    x  <- model_frame(object$terms, data)
    x  <- model_matrix(object$terms, x)
    
    if(se_fit) {
      
      sigma   <- summary(object)$sigma
      sdm_inv <- object$qr$sdm_inv()
      
      std_error <- function(obs) {
        sigma * sqrt(t(obs) %*% sdm_inv %*% obs)
      }
      
      return(list(
        fit = x %*% coef(object),
        se  = apply(x, 1, std_error)))
    }
    
    x %*% coef(object)
    
  }
  
  if(as_function) {
    return(fitfxn)
  }
  
  fitfxn(data)
  
}


#' @rdname predict
#' @export
predict.oomglm <- function(object, 
                           data,
                           se_fit = FALSE,
                           interval = c("none", "confidence", "prediction"),
                           type = c("link", "response"),
                           as_function = FALSE,
                           ...) {

  if(!as_function && is.null(data)){
    stop("`data` must be provided if `as_function` is FALSE")
  }
  
  link_pred <- function(data) {
    
    x  <- model_frame(object$terms, data)
    x  <- model_matrix(object$terms, x)
    
    if(se_fit) {
      
      sigma   <- summary(object)$sigma
      sdm_inv <- object$qr$sdm_inv()
      
      std_error <- function(obs) {
        sigma * sqrt(t(obs) %*% sdm_inv %*% obs)
      }
      
      return(list(
        fit = x %*% coef(object),
        se  = apply(x, 1, std_error)))
    }
    
    x %*% coef(object)
    
  }
  
  if(match.arg(type) == "response") {
    
    fam      <- family(object)
    linkinv  <- fam$linkinv
    mu_eta   <- fam$mu.eta
    
    resp_pred <- function(data) {
      
      pred <- link_pred(data)
      
      if(se_fit) {
        y  <- linkinv(pred$fit)
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

