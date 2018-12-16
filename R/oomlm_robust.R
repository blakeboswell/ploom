
#' initialize generalized linear model with robust standard errors
#' 
#' @md
#' @param formula a symbolic description of the model to be fitted
#'   of class `formula`.
#' @param weights A one-sided, single term `formula` specifying weights.
#' @keywords internal
init_oomlm_robust <- function(formula, weights = NULL) {
  
  object          <- init_oomlm(formula, weights)
  object$sandwich <- list(xy = NULL)
  class(object)   <- c(class(object), "oomlm_robust")
  
  object
  
}


#' Initialize Updating Linear model
#' 
#' Perform linear regression via Alan Miller's bounded memory QR
#'   factorization algorithm which enables models with `p` variables
#'   to be fit in `p^2` memory.
#' 
#' @md
#' @param formula a symbolic description of the model to be fitted of class
#'   `formula`.
#' @param data an optional `oomfeed`, `tibble`, `dataframe`, `list` or 
#'   `environment`.
#' @param weights a one-sided, single term `formula` specifying weights.
#' @param ... ignored.
#' @details `oomlm` initializes an object of class `oomlm`. `oomlm` objects
#'   are intended to be iteratively updated with new data via the function 
#'   [update()]. If `data` is provided to the `oolm` function call, an 
#'   [update()] round will be performed on initialization.
#'
#'   The provided `formula` must not contain any data-dependent terms to ensure
#'   consistency across calls to [update()]. Factors are permitted, but the
#'   levels of the factor must be the same across all data chunks. Empty factor
#'   levels are accepted.
#'   
#' @return A `oomlm` object is perpetually in an _in-progress_ state. It is up
#'   to the user to know when fitting is complete.  Therefore, only basic model
#'   characteristics are provided as values with the `oolm` object. Statistics
#'   are available on demand via `summary` and extractor functions.
#'
#' \item{n}{the number of observations processed.}
#' \item{df.residual}{the residual degrees of freedom.}
#' \item{formula}{the [stats::formula()] object specifying the linear model.}
#' \item{terms}{the [stats::terms()] object specifying the terms of the linear model.}
#' \item{weights}{a one-sided, single term `formula` specifying weights.}
#' \item{call}{the matched call.}
#' @seealso [oomglm()]
#' @aliases AIC.oomlm coef.oomlm deviance.oomlm family.oomlm formula.oomlm
#'   predict.oomlm print.oomlm print.summary.oomlm summary.oomlm vcov.oomlm
#' @export
#' @name oomlm
#' @examples \donttest{
#' # The `ploom` linear model, `oomlm`, is similar to base `lm` for fitting
#' # in-memory data.
#' 
#' x <- oomlm(mpg ~ cyl + disp, data = mtcars)
#' 
#' # Models are initalized with a call to `oomlm()` and updated with `update()`.
#' # The intended pattern is to initialize models without referencing data,
#' # then call `update()` on each data chunk.
#' 
#' # proxy for big data feed 
#' chunks  <- oomfeed(mtcars, chunk_size = 10)
#' 
#' # initialize the model
#' x <- oomlm(mpg ~ cyl + disp)
#' 
#' # iteratively update model with data chunks
#' while(!is.null(chunk <- chunks())) {
#'   x <- update(x, chunk)
#' }
#' 
#' summary(x)
#' 
#' }
oomlm_robust <- function(formula, data = NULL, weights = NULL, ...) {
  
  object <- init_oomlm_robust(formula, weights)
  
  if(!is.null(data)) {
    object <- update(object, data)
  }
  
  object
  
}


#' @export
#' @rdname update
update.oomlm_robust <- function(object, data, ...) {
  
  chunk <- unpack_oomchunk(object, data)
  
  if(is.null(object$assign)) {
    object$assign <- chunk$assign
    object$names  <- colnames(chunk$data)
  }
  
  if(is.null(object$qr)) {
    qr <- new_bounded_qr(chunk$p)
  } else {
    qr <- object$qr
  }
  
  object$qr <- update(qr,
                      chunk$data,
                      chunk$response - chunk$offset,
                      chunk$weights)
  
  object$n            <- object$qr$num_obs
  object$df.residual  <- object$n - chunk$p
  
  object$sandwich$xy <-
    update_sandwich(obj$sandwich$xy,
                    chunk$data,
                    chunk$n,
                    chunk$p,
                    chunk$response,
                    chunk$offset,
                    chunk$weights)

  
  object
  
}

