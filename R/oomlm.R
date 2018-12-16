#' Initialize Updating Linear Model
#' 
#' @noRd
#' @description
#' Performs the details of intializing `oomlm` object called by
#' `oomlm` function.
#' 
#' @param formula a symbolic description of the model to be fitted of class
#'   `formula`.
#' @param weights A one-sided, single term `formula` specifying weights.
#' @keywords internal
init_oomlm <- function(formula, weights  = NULL) {
  
  if(!is.null(weights) && !inherits(weights, "formula")) {
    stop("`weights` must be a formula")
  }
  
  object <- list(
    formula      = formula,
    terms        = terms(formula),
    weights      = weights,
    call         = sys.call(-1),
    n            = 0,
    df.residual  = NULL,
    qr           = NULL,
    names        = NULL,
    assign       = NULL
  )
  
  class(object) <- "oomlm"
  object
  
}


#' Fit Updating Linear Model to more data
#' 
#' @md
#' @description
#' Update `oomlm` linear model fit with new data. 
#' 
#' @param object `oomlm` object to be updated.
#' @param data an optional `oomfeed`, `tibble`, `dataframe`, `list`
#'   or `environment`.
#' @param ... ignored
#' @seealso [oomlm()]
#' @export
update.oomlm <- function(object, data, ...) {
  
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
  
  object
  
}


#' Initialize Updating Linear model
#' 
#' @md
#' @description
#' Perform linear regression via Alan Miller's bounded memory QR
#'   factorization algorithm which enables models with `p` variables
#'   to be fit in `p^2` memory.
#' 
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
#' @export
oomlm <- function(formula, data = NULL, weights  = NULL, ...) {
  
  object <- init_oomlm(formula, weights)
  
  if(!is.null(data)) {
    object <- update(object, data)
  }
  
  object
  
}


#' @export
print.oomlm <- function(x,
                        digits = max(3L, getOption("digits") - 3L),
                        ...) {
  
  cat("\nCall:  ",
      paste(deparse(x$call), sep = "\n", collapse = "\n"),
      "\n\n",
      sep = "")
  
  beta <- coef(x)
  
  if(length(beta)) {
    cat("Coefficients:\n")
    print.default(
      format(beta, digits = digits),
      print.gap = 2L,
      quote     = FALSE)
  } else {
    cat("No coefficients\n")
  }
  
  cat("\n")
  cat("Observations included: ", x$n, "\n")
  
  invisible(x)
  
}

