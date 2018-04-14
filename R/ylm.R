#' @include yotta_shared.R


#' Initialize Updating Linear Regression Model
#' 
#' @description
#' Performs the details of intializing `ylm` object called by
#' `ylm` function.
#' 
#' @param formula a symbolic description of the model to be fitted of class `formula`
#' @param weights A one-sided, single term `formula` specifying weights
#' @param sandwich TRUE to compute the Huber/White sandwich covariance matrix
#' 
#' @keywords internal
init_ylm <- function(formula,
                     weights  = NULL,
                     sandwich = FALSE) {
  
  if(!is.null(weights) && !inherits(weights, "formula")) {
    stop("`weights` must be a formula")
  }
  
  if(sandwich) {
    xy <- list(xy = NULL)
  } else {
    xy <- NULL
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
    sandwich     = xy,
    weights      = weights,
    pweights     = 0,
    zero_weights = 0
  )
  
  class(obj) <- 'ylm'
  obj
  
}


#' Update Updating Linear Regression Model with more data
#' 
#' @description
#' Update `ylm` linear model fit with new data 
#' 
#' @param obj `ylm` object to be updated
#' @param data an optional `oomfeed`, `tibble`, `dataframe`, `list` or `environment`
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
  
  if(!is.null(obj$sandwich)) {
    obj$sandwich$xy <-
      update_sandwich(obj$sandwich$xy,
                      chunk$data,
                      chunk$n,
                      chunk$p,
                      chunk$response,
                      chunk$weights)
  }
  
  
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
#' @description
#' Apply Alan Miller's bounded memory QR factorization algorithm to perform
#' linear regression on `p` covariates using only `p^2` memory.
#' 
#' @param formula a symbolic description of the model to be fitted of class `formula`
#' @param data an optional `oomfeed`, `tibble`, `dataframe`, `list` or `environment`
#' @param weights A one-sided, single term `formula` specifying weights
#' @param sandwich TRUE to compute the Huber/White sandwich covariance matrix
#'   (uses `p^4` memory rather than `p^2`)
#' @details The model formula must not contain any data-dependent terms, as
#'   these will not be consistent when updated. Factors are permitted, but 
#'   the levels of the factor must be the same across all data chunks. 
#'   Empty factor levels are accepted.
#' @return `ylm` model object that can be updated with more data
#'   via [update_ylm][yotta::update_ylm]
#' @examples
#' # The function `ylm` is similar to base `lm` for fitting in-memory data.
#'
#' w <- ylm(mpg ~ cyl + disp, data = mtcars)
#'
#' # Models are initalized with a call to `ylm` and updated with
#' # `update_ylm`. The intended pattern is to initialize a model with the formula
#' # only and then to reference the data through iterative (identical) calls over
#' # subsets of the data to be fitted.
#' 
#' # proxy for data feed
#' chunks  <- purrr::pmap(mtcars, list)
#' 
#' # initialize the model
#' x <- ylm(mpg ~ cyl + disp)
#' 
#' # iteratively update model with data chunks
#' for(chunk in chunks) {
#'   update_ylm(x, chunk)
#' }
#'
#' # Separating model initialization and processing of the first data chunk
#' # enables functional patterns like `reduce` to take the place of loops.
#' # The below example is equivalent to the above `for` loop.
#' 
#' # avoid loops altogether with `purrr::reduce`
#' y <- purrr::reduce(chunks, update_ylm, .init = ylm(mpg ~ cyl + disp))
#' 
#' # For maximum flexibility, `yotta` also supports processing the first chunk of 
#' # data on initialization similar to [`biglm`](https://github.com/cran/biglm).
#'
#' # initialize model and process first chunk of data
#' z  <- ylm(mpg ~ cyl + disp, chunks[[1]])
#' 
#' # iteratively update model with additional data chunks
#' for(chunk in tail(chunks, -1)) {
#'   z <- update_ylm(x, data = chunk)
#' }
#'
#' @export
ylm <- function(formula,
                data     = NULL,
                weights  = NULL,
                sandwich = FALSE) {
  
  obj <- init_ylm(formula, weights, sandwich)
  
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

