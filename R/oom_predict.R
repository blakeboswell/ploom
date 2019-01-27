
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
  
  fn <- function(new_data) {
    
    x   <- model_frame(object$terms, new_data)
    x   <- model_matrix(object$terms, x)
    fit <- x %*% coef(object)
    
    if(se_fit) {
      
      rss    <- object$qr$rss_full
      dof    <- object$df.residual
      vcov_y <- vcov(object)
      res_scale <- rss / dof
      
      var_y <- apply(x, 1, function(x){
        tcrossprod(crossprod(x, vcov_y), x)
      })
      
      if(!is.null(interval)) {
        predi <- res_scale * (interval %in% "prediction")
        intv  <- sqrt(predi + var_y)
        tval  <- qt((1 - level)/2, dof)
        fit   <- cbind(fit, fit + intv * tval, fit - intv * tval)
        colnames(fit) <- c(".pred", "lwr", "upr")
      }
      
      rval <- list(
        fit    = drop(fit),
        se.fit = drop(sqrt(var_y))
      )
      
      if(return_dof){
        rval[["df"]] <- dof
      }
      
      rval[["residual.scale"]] <- sqrt(res_scale)
      
      return(rval)
      
    }
    
    drop(fit)
    
  }
  
  if(as_function) {
    return(fn)
  }
  
  fn(new_data)
  
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
  
  link_fn <- predict.oomlm(object,
                           se_fit = se_fit,
                           as_function = TRUE)
  
  if(match.arg(type) == "response") {
    
    fam      <- family(object)
    linkinv  <- fam$linkinv
    mu_eta   <- fam$mu.eta
    
    resp_fn <- function(new_data) {
      
      pred  <- link_fn(new_data)
      if(se_fit) {
        y  <- linkinv(pred$fit)
        return(list(
          fit = y,
          se  = pred$se.fit %*% mu_eta(y)^2))
      }
      
      linkinv(pred$fit)
      
    }
    
    fn <- resp_fn
    
  } else {
    fn <- link_fn
  }
  
  if(as_function) {
    return(fn)
  }
  
  fn(new_data)
  
}



#' calculate residuals for oomlm
#'
#' @param object `oomlm` model
#' @param data data to calculate residuals
#' @param as_function if TRUE a function with only a `data` argument is returned
#'   for subsequent residual calculations
#' 
#' @export
oomlm_residuals <- function(object, data, as_function = FALSE) {

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


#' calculate residuals for oomglm
#'
#' @param object `oomlm` model
#' @param data data to calculate residuals
#' @param type residual calculation method
#' @param as_function if TRUE a function with only a `data` argument is returned
#'   for subsequent residual calculations
#' 
#' @export
oomglm_residuals <- function(object,
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
      pearson = (y - yhat) * sqrt(wts) / sqrt(fam$variance(yhat)),
      working = r,
      response = y - yhat
    )
    
    drop(res)

  }
  
  if(as_function) {
    return(fn)  
  }
  
  fn(data)

}
