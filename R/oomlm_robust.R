
#' initialize generalized linear model with robust standard errors
#' 
#' @md
#' @param formula a symbolic description of the model to be fitted
#'   of class `formula`.
#' @param weights A one-sided, single term `formula` specifying weights.
#' @param se_type method for robust standard error estimation
#' @keywords internal
init_oomlm_robust <- function(formula, weights = NULL, se_type) {
  
  object          <- init_oomlm(formula, weights)
  
  object$call     <- sys.call(-1)
  object$sandwich <- list(xy = NULL)
  object$se_type  <- se_type
  class(object)   <- c("oomlm_robust", class(object))
  
  object
  
}


#' Out of Memory Linear model with robust standard errors
#' 
#' Perform linear regression via Alan Miller's bounded memory QR
#'   factorization algorithm enables models with `p` variables with 
#'   `p` variables  to be fit with robust standard errors in `p^4` memory.
#' 
#' @md
#' @param formula a symbolic description of the model to be fitted of class
#'   `formula`.
#' @param data an optional `oom_data`, `tibble`, `data.frame`, `list` or 
#'   `environment`.
#' @param weights a one-sided, single term `formula` specifying weights.
#' @param se_type string method for robust standard error estimation.
#'   "HC0", "HC1", "stata", or "classical". see [details].  default "HC1".
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
#' @aliases vcov.oomlm_robust
#' @export
#' @name oomlm_robust
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
#' chunks  <- oom_data(mtcars, chunk_size = 10)
#' 
#' # initialize the model
#' x <- oomlm_robust(mpg ~ cyl + disp)
#' 
#' # iteratively update model with data chunks
#' while(!is.null(chunk <- chunks())) {
#'   x <- update(x, chunk)
#' }
#' 
#' summary(x)
#' 
#' }
oomlm_robust <- function(formula,
                         data    = NULL,
                         weights = NULL,
                         se_type = "HC1", ...) {
  
  if(!(se_type %in% c("HC0", "HC1", "stata", "classical"))) {
    msg <- c('`se_type` must be one of "HC0", "HC1", "stata", or "classical"',
             "See ?oomlm_robust for details.")
    stop(paste(msg, collapse = "\n"))
  }
  
  object <- init_oomlm_robust(formula, weights, se_type)
  
  if(!is.null(data)) {
    object <- update(object, data)
  }
  
  object
  
}
