
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
#'   "stata", or "classical".  default "HC1".
#' @param ... ignored.
#' @details `oomglm_robust` initializes an object of class `oomlm`. `oomlm` objects
#'   are intended to be iteratively updated with new data via the function 
#'   [update()]. If `data` is provided to the `oolm` function call, an 
#'   [update()] round will be performed on initialization.
#'
#'   The provided `formula` must not contain any data-dependent terms to ensure
#'   consistency across calls to [update()]. Factors are permitted, but the
#'   levels of the factor must be the same across all data chunks. Empty factor
#'   levels are accepted.
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
#' \item{formula}{the [`formula()`] object specifying the linear model.}
#' \item{family}{a [`family()`] object describing the error distribution
#'   and link function used in the model.}
#' \item{terms}{The [`terms()`] object used.}
#' \item{weights}{The weights [`formula()`] provided to the model.}
#' \item{call}{The matched call.}
#' @seealso [oomlm_robust()]
#' @aliases vcov.oomglm_robust
#' @name oomglm_robust
#' @export
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
