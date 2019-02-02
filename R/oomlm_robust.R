

#' Internal. Initialize generalized linear model with robust standard errors
#' 
#' @md
#' @param formula A symbolic description of the model to be fitted
#'  of class `formula`.
#' @param weights A one-sided, single term `formula` specifying weights.
#' @param se_type Method for robust standard error estimation.
#' 
#' @keywords internal
init_oomlm_robust <- function(formula, weights, se_type) {
  
  object          <- init_oomlm(formula, weights)
  
  object$call     <- sys.call(-1)
  object$sandwich <- list(xy = NULL)
  object$se_type  <- se_type
  class(object)   <- c("oomlm_robust", class(object))
  
  object
  
}


#' Out of memory Linear model with robust standard errors
#' 
#' Perform memory-efficient generalized linear regression using 
#' the AS274 bounded memory QR factorization algorithm and estimate
#' robust standard errors.
#' 
#' @md
#' @param formula A symbolic description of the model to be fitted of class
#'  `formula`.
#' @param weights A one-sided, single term `formula` specifying weights.
#' @param se_type Indicates what Standard Error method to use: "HC0", "HC1"
#'  "stata", or "classical".
#' @param ... Ignored.
#' @details
#'  The provided `formula` must not contain any data-dependent terms to ensure
#'  consistency across calls to `fit()`. Factors are permitted, but the
#'  levels of the factor must be the same across all data chunks. Empty factor
#'  levels are accepted.
#'   
#' @return A `oomlm_robust` model is perpetually in an _in-progress_ state. It is up
#'  to the user to know when fitting is complete. Therefore, only basic
#'  model characteristics are provided as values. Statistics are available on 
#'  demand via summary and extractor functions.
#'
#' \item{n}{The number of observations processed.}
#' \item{df.residual}{The residual degrees of freedom.}
#' \item{formula}{The `formula` object specifying the linear model.}
#' \item{terms}{The `terms` object specifying the terms of the linear model.}
#' \item{weights}{a one-sided, single term `formula` specifying weights.}
#' \item{call}{The matched call.}
#' @seealso [oomlm()]
#' @aliases vcov.oomlm_robust
#' @name oomlm_robust
#' @export
oomlm_robust <- function(formula,
                         weights = NULL,
                         se_type = "HC1", ...) {
  
  if(!(se_type %in% c("HC0", "HC1", "stata", "classical"))) {
    msg <- c('`se_type` must be one of "HC0", "HC1", "stata", or "classical"',
             "See ?oomlm_robust for details.")
    stop(paste(msg, collapse = "\n"))
  }
  
  init_oomlm_robust(formula, weights, se_type)
  
}
