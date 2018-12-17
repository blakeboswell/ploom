
#' Initialize Updating Generalized Linear Regression Model
#' with robust standard errors
#'
#' 
#' @md
#' @param formula a symbolic description of the model to be fitted of class
#'  `formula`.
#' @param family a `glm` family object.
#' @param weights a one-sided, single term `formula` specifying weights. 
#' @param start starting values for the parameters in the linear predictor.
#' @param se_type type of robust standard errors
#' @keywords internal
init_oomglm_robust <- function(formula,
                               family,
                               weights,
                               start,
                               se_type) {
  
  object <- init_oomglm(formula, family, weights, start)
  
  object$call     <- sys.call(-1)
  object$sandwich <- list(xy = NULL)
  object$se_type  <- se_type
  class(object)   <- c("oomglm_robust", class(object))
  
  object
  
}


#' Out of Memory Generalized Linear model with robust standard errors
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
#' @param family A `glm` family object.
#' @param weights a one-sided, single term `formula` specifying weights.
#' @param start starting values for the parameters in the linear predictor.
#' @param se_type string indicating what se type to usecan be "HC0", "HC1"
#'   "stata", or "classical". see [details].  default "HC1".
#' @param ... ignored.
#' 
#' @details
#' `oomglm` initializes an object of class `oomglm` inheriting from the
#'   class `oomlm`. `oomglm` objects are intended to be iteratively 
#'   updated with new data via calls to [update()]. Iterative fitting
#'   over all data updates are performed with the function [iter_weight()].
#'   If `data` is provided to the `oomglm()` function call, an `update()` round 
#'   will be performed on initialization.
#' 
#'   A `oomglm` object can be in various states of fit depending on the number
#'   of seen observations and rounds of IRLS that have been performed.
#'   It is important to view the model within the context of:
#'   the number of observations processed per round of IRLS (`n`);
#'   the number of IRLS iterations that have been performed (`iter`);
#'   and if the IRLS algorithm has converged (`converged`).
#'
#' @return It is up to the user to know when fitting is complete.
#'   Therefore, only basic model characteristics are provided as values with 
#'   the `oomglm` object. Statistics are available on demand via `summary` and 
#'   extractor functions.
#'
#' \item{converged}{Indicates if the IRLS algorithm has converged.}
#' \item{iter}{The number of iterations of IRLS performed.}
#' \item{n}{The number observations processed per round of IRLS.}
#' \item{df.residual}{The residual degrees of freedom.}
#' \item{df.null}{The residual degrees of freedom.}
#' \item{formula}{the [`stats::formula()`] object specifying the linear model.}
#' \item{family}{a [`stats::family()`] object describing the error distribution
#'   and link function used in the model.}
#' \item{terms}{The [`stats::terms()`] object used.}
#' \item{weights}{The weights [`stats::formula()`] provided to the model.}
#' \item{call}{The matched call.}
#' @seealso [oomlm()]
#' @aliases predict.oomglm print.oomglm print.summary.oomglm
#'   summary.oomglm
#' @export
#' @name oomglm
#' @examples \donttest{
#' # The `oomglm()` function fits generalized linear models via 
#' # Iteratively Weighted Least Squares (IWLS).  
#'
#' # When fitting in-memory data the process is similar to `oomlm()` but we use the 
#' # function `iter_weight()` instead of `update()`. `iter_weight()` fits the model
#' # via iterative passes over the data until convergence.
#' 
#' # initialize the model
#' x <- oomglm(mpg ~ cyl + disp)
#' 
#' # re-weight 8 times or until convergence
#' x <- iter_weight(x, mtcars, max_iter = 8)
#' 
#' # To fit data in chunks, use `oom_data()`:
#' 
#' # initialize the model
#' x    <- oomglm(mpg ~ cyl + disp)
#' feed <- oom_data(mtcars, chunk_size = 10)
#' 
#' # iteratively reweight model
#' x <- iter_weight(x, feed, max_iter = 8)
#' 
#' # The `iter_weight()` process can also be implemented directly with
#' # component `ploom` functions:
#' 
#' x    <- oomglm(mpg ~ cyl + disp)
#' feed <- oom_data(mtcars, chunk_size = 10)
#' 
#' # a first pass over the data
#' x <- init_weight(x)
#' x <- weight(x, feed)
#' x <- end_weight(x)
#' x
#' 
#' # a second pass over the data
#' x <- init_weight(x)
#' x <- weight(x, feed)
#' x <- end_weight(x)
#' x
#' 
#' # This is meant to be useful when debugging / evaluating models with long 
#' # runtimes by exposing the individual steps of the model process for inspection. 
#'
#' }
oomglm_robust <- function(formula,
                          data     = NULL,
                          family   = gaussian(),
                          weights  = NULL,
                          start    = NULL,
                          se_type  = "H1",
                          ...) {
  
  if(!(se_type %in% c("HC0", "HC1", "stata", "classical"))) {
    msg <- c('`se_type` must be one of "HC0", "HC1", "stata", or "classical"',
             "See ?oomlm_robust for details.")
    stop(paste(msg, collapse = "\n"))
  }
  
  object <- init_oomglm_robust(formula,
                               family,
                               weights,
                               start,
                               se_type)
  
  if(!is.null(data)) {
    object <- weight(object, data)
  }
  
  object
  
}
