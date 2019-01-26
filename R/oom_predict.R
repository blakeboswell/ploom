#' Predict values using `oomlm()` and `oomglm()` models
#'
#' @param object object inheriting from class `oomlm`
#' @param new_data observations for prediction
#' @param se_fit indicates if the standard error of predicted means should
#'   be returned
#' @param interval type of interval calculation
#' @param level tolerance/confidence level
#' @param type the type of prediction, one of "reponse" or "link"
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
                          new_data = NULL,
                          se_fit   = FALSE,
                          interval = NULL,
                          level    = 0.95,
                          as_function = FALSE,
                          ...) {
  
  if(!as_function && is.null(new_data)){
    stop("`new_data` must be provided if `as_function` is FALSE")
  }
  
  return_dof <- all(class(object) %in% c("oomlm"))
  
  fitfxn <- function(new_data) {
    
    x   <- model_frame(object$terms, new_data)
    x   <- model_matrix(object$terms, x)
    fit <- x %*% coef(object)
    
    if(se_fit) {
      
      sobj     <- summary(object)
      sigma    <- sobj$sigma
      sdm_inv  <- object$qr$sdm_inv()
      pi_level <- if(is.null(interval)) 0 else (interval %in% "prediction")
      dof      <- sobj$df[2]
      
      std_error <- function(obs, pi_level = 0) {
        sigma * sqrt(pi_level + t(obs) %*% sdm_inv %*% obs)
      }
      
      se    <- apply(x, 1, std_error)
      
      if(!is.null(interval)) {
        
        pse <- if (pi_level > 0) {
          apply(x, 1, std_error, pi_level = 1)
        } else {
          se
        }
        
        tfrac <- qt((1 - level)/2, dof)
        fit   <- cbind(fit, fit + pse * tfrac, fit - pse * tfrac)
        colnames(fit) <- c(".pred", "lwr", "upr")
      }
      
      rss       <- object$qr$rss()
      dof       <- object$df.residual
      res_scale <- sqrt(tail(rss, 1) / dof)
      
      rval <- list(fit = drop(fit), se = drop(se))
      if(return_dof){
        rval[["df"]] <- dof
      }
      
      rval[["residual_scale"]] <- res_scale
      
      return(rval)
      
    }
    
    drop(fit)
    
  }
  
  if(as_function) {
    return(fitfxn)
  }
  
  fitfxn(new_data)
  
}


#' @rdname predict
#' @export
predict.oomglm <- function(object, 
                           new_data,
                           type   = c("link", "response"),
                           se_fit = FALSE,
                           as_function = FALSE,
                           ...) {

  if(!as_function && is.null(new_data)){
    stop("`new_data` must be provided if `as_function` is FALSE")
  }
  
  link_pred <- predict.oomlm(object,
                             se_fit = se_fit,
                             as_function = TRUE)
  
  if(match.arg(type) == "response") {
    
    fam      <- family(object)
    linkinv  <- fam$linkinv
    mu_eta   <- fam$mu.eta
    
    resp_pred <- function(new_data) {
      
      pred  <- link_pred(new_data)
      if(se_fit) {
        y  <- linkinv(pred$fit)
        return(list(
          fit = y,
          se  = pred$se %*% mu_eta(y)^2))
      }
      
      linkinv(pred$fit)
      
    }
    
    fitfxn <- resp_pred
    
  } else {
    fitfxn <- link_pred
  }
  
  if(as_function) {
    return(fitfxn)
  }
  
  fitfxn(new_data)
  
}

