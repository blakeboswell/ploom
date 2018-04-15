#' @include yotta_shared.R


#' Initialize Updating Linear Regression Model
#' 
#' @noRd
#' @description
#' Performs the details of intializing `ylm` object called by
#' `ylm` function.
#' 
#' @param formula a symbolic description of the model to be fitted of class `formula`.
#' @param weights A one-sided, single term `formula` specifying weights.
#' 
#' @keywords internal
init_ylm <- function(formula, weights  = NULL) {
  
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
  
  class(obj) <- 'ylm'
  obj
  
}


#' Update Updating Linear Regression Model with more data
#' 
#' @md
#' @description
#' Update `ylm` linear model fit with new data. 
#' 
#' @param obj `ylm` object to be updated.
#' @param data an optional `oomfeed`, `tibble`, `dataframe`, `list` or `environment`.
#' 
#' @export
update.ylm <- function(obj, data) {
  
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


#' Initialize Updating Linear Regression model
#' 
#' @md
#' @description
#' Perform linear regression via Alan Miller's bounded memory QR
#'   factorization algorithm which enables models with `p` variables
#'   to be fit in `p^2` memory.
#' 
#' @param formula a symbolic description of the model to be fitted of class `formula`.
#' @param data an optional `oomfeed`, `tibble`, `dataframe`, `list` or `environment`.
#' @param weights a one-sided, single term `formula` specifying weights.
#' @param ... ignored.
#' @details The model formula must not contain any data-dependent terms, as
#'   these will not be consistent when updated. Factors are permitted, but 
#'   the levels of the factor must be the same across all data chunks. 
#'   Empty factor levels are accepted.
#'
#' @return `ylm` initializes an object of class `ylm`. If `data` is missing,
#'   the `ylm` object will not be fit on initialization. `ylm` objects can
#'   be iteratively updated with new data via the function `update`. If 
#'   `data` is provided, an `update` will be performed on initialization.
#'  
#' @seealso [yglm()]
#' 
#' @examples
#' # The function `ylm` is similar to base `lm` for fitting in-memory data.
#' w <- ylm(mpg ~ cyl + disp, data = mtcars)
#'
#' # Models are initalized with a call to `ylm` and updated with
#' # `update`. The recommended pattern is to initialize a model without providing
#' # data, then feed the data via calls to `update`.  For example:
#' 
#' # proxy for data feed
#' chunks  <- purrr::pmap(mtcars, list)
#' 
#' # initialize the model
#' x <- ylm(mpg ~ cyl + disp)
#' 
#' # iteratively update model with data chunks
#' for(chunk in chunks) {
#'   update(x, chunk)
#' }
#'
#' # Separating model initialization and processing of data 
#' # enables functional patterns like `reduce` to take the place of loops.
#' y <- purrr::reduce(chunks, update, .init = ylm(mpg ~ cyl + disp))
#' 
#' @export
ylm <- function(formula, data = NULL, weights  = NULL, ...) {
  
  obj <- init_ylm(formula, weights)
  
  if(!is.null(data)) {
    obj <- update(obj, data)
  }
  
  obj
  
}


#' @export
print.ylm <- function(x,
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

