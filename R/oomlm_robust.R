

#' Initialize generalized linear model with robust standard errors
#' 
#' @md
#' @param formula a symbolic description of the model to be fitted
#'  of class `formula`.
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


#' Out of memory Linear model with robust standard errors
#' 
#' Perform linear regression via Alan Miller's bounded memory QR
#' factorization algorithm and estimate robust standard errors.
#' 
#' @md
#' @param formula a symbolic description of the model to be fitted of class
#'  [formula()].
#' @param data an optional [oomdata_tbl()], [oomdata_dbi()], [oomdata_con()],
#'  [tibble()], [data.frame()], or [list()] of observations to fit
#' @param weights a one-sided, single term [formula()] specifying weights.
#' @param se_type string indicating what `se` type to use: "HC0", "HC1"
#'  "stata", or "classical"
#' @param ... ignored.
#' @details
#'  The provided [formula()] must not contain any data-dependent terms to ensure
#'  consistency across calls to [update()]. Factors are permitted, but the
#'  levels of the factor must be the same across all data chunks. Empty factor
#'  levels are accepted.
#'   
#' @return A [oomlm_robust()] model is perpetually in an _in-progress_ state. It is up
#'  to the user to know when fitting is complete. Therefore, only basic
#'  model characteristics are provided as values. Statistics are available on 
#'  demand via `summary` and `extractor` functions.
#'
#' \item{n}{the number of observations processed.}
#' \item{df.residual}{the residual degrees of freedom.}
#' \item{formula}{the [formula()] object specifying the linear model.}
#' \item{terms}{the [terms()] object specifying the terms of the linear model.}
#' \item{weights}{a one-sided, single term [formula()] specifying weights.}
#' \item{call}{the matched call.}
#' @seealso [oomlm()]
#' @aliases vcov.oomlm_robust
#' @name oomlm_robust
#' @export
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
