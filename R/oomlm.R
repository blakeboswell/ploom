#' @include ploom_shared.R


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
#' 
#' @keywords internal
init_oomlm <- function(formula, weights  = NULL) {
  
  if(!is.null(weights) && !inherits(weights, "formula")) {
    stop("`weights` must be a formula")
  }
  
  obj <- list(
    call         = sys.call(-1),
    qr           = NULL,
    assign       = NULL,
    terms        = terms(formula),
    n            = 0,
    p            = NULL,
    names        = NULL,
    df.resid     = NULL,
    weights      = weights,
    pweights     = 0,
    zero_weights = 0
  )
  
  class(obj) <- "oomlm"
  obj
  
}


#' Fit Updating Linear Model to more data
#' 
#' @md
#' @description
#' Update `oomlm` linear model fit with new data. 
#' 
#' @param obj `oomlm` object to be updated.
#' @param data an optional `oomfeed`, `tibble`, `dataframe`, `list`
#'   or `environment`.
#'
#' @seealso [oomlm()]
#' @examples
#' #' # create simple example data chunks
#' chunks  <- purrr::pmap(mtcars, list)
#' 
#' # initialize the model
#' x <- oomlm(mpg ~ cyl + disp)
#' 
#' # iteratively update model with chunks
#' for(chunk in chunks) {
#'   update(x, chunk)
#' }
#'
#' # `oomlm` models can be fit with functional patterns like `purrr::reduce`.
#' y <- purrr::reduce(chunks, update, .init = oomlm(mpg ~ cyl + disp))
#' 
#' @export
update.oomlm <- function(obj, data) {
  
  chunk <- unpack_oomchunk(obj, data)
  
  if(is.null(obj$assign)) {
    obj$assign <- chunk$assign
  }
  
  if(is.null(obj$qr)) {
    qr <- new_bounded_qr(chunk$p)
  } else {
    qr <- obj$qr
  }
  
  obj$qr <- update(qr,
                   chunk$data,
                   chunk$response - chunk$offset,
                   chunk$weights)
  
  zero_wts  <- chunk$weights == 0
  
  obj$n            <- obj$n + chunk$n - sum(zero_wts)
  obj$names        <- colnames(chunk$data)
  obj$df.resid     <- obj$n - chunk$p
  obj$pweights     <- obj$pweights + sum(log(chunk$weights[!zero_wts]))
  obj$zero_weights <- obj$zero_weights + sum(zero_wts)
  
  obj
  
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
#' @details `oomlm` initializes an object of class `oomlm`. If `data` is
#'   missing, the `oomlm` object will *not* be fit on initialization. 
#'   `oomlm` objects are intended to be iteratively updated with new data via
#'   the function [update()]. If  `data` is provided, an [update()] will be
#'   performed on initialization.
#'
#'   `formula` must not contain any data-dependent terms to ensure consistency
#'   across calls to [update()]. Factors are permitted, but the levels of the
#'   factor must be the same across all data chunks. Empty factor levels are
#'   accepted.
#'   
#' @return A `oomlm` object can be in an in-progress state. Therefore, it is 
#'   important to consider the number of observations processed when assessing
#'   the return values.
#'
#' \item{coefficients}{a named vector of coefficients.}
#' \item{rank}{the numeric rank of the fitted linear model.}
#' \item{weights}{a one-sided, single term `formula` specifying weights.}
#' \item{df.residual}{the residual degrees of freedom.}
#' \item{call}{the matched call.}
#' \item{terms}{the [stats::terms()] object used.}
#' \item{n}{the number of observations processed.}
#' 
#' @seealso [oomglm()]
#' @examples
#' # The function `oomlm` is similar to base `lm` for fitting in-memory data.
#' w <- oomlm(mpg ~ cyl + disp, data = mtcars)
#'
#' # Models are initalized with a call to `oomlm` and updated with `update`.
#' # The recommended pattern is to initialize a model without providing data
#' # then feed the data via iterative calls to `update`. For example:
#' 
#' # create simple example data chunks
#' chunks  <- purrr::pmap(mtcars, list)
#' 
#' # initialize the model
#' x <- oomlm(mpg ~ cyl + disp)
#' 
#' # iteratively update model with chunks
#' for(chunk in chunks) {
#'   update(x, chunk)
#' }
#'
#' # `oomlm` models can be fit via functional patterns like `purrr::reduce`.
#' y <- reduce(chunks, update, .init = oomlm(mpg ~ cyl + disp))
#' 
#' @export
oomlm <- function(formula, data = NULL, weights  = NULL, ...) {
  
  obj <- init_oomlm(formula, weights)
  
  if(!is.null(data)) {
    obj <- update(obj, data)
  }
  
  obj
  
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

