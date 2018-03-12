
#' Updating Linear Regression model
#' 
#' @description
#' 
#' 
#' @param formula A model formula: a symbolic description of the
#'   model to be fitted.
#' @param data an optional data frame, list or environment
#'   (or object coercible by as.data.frame to a data frame)
#' @param weights A one-sided, single term formula specifying weights
#' @param sandwich TRUE to compute the Huber/White sandwich covariance matrix
#'   (uses `p^4` memory rather than `p^2`)
#' @details The model formula must not contain any data-dependent terms, as
#'   these will not be consistent when updated. Factors are permitted, but 
#'   the levels of the factor must be the same across all data chunks. 
#'   Empty factor levels are accepted.
#' @return `oomlm` model object that can be updated with more data
#'   via [update_oomlm][ploom::update_oomlm]
#' @examples
#' # The function `oomlm` is similar to base `lm` for fitting in-memory data.
#'
#' w <- oomlm(mpg ~ cyl + disp, data = mtcars)
#'
#' # Models are initalized with a call to `oomlm` and updated with
#' # `update_oomlm`. The recommended pattern is to initialize models without
#' # referencing the data, then call `update_oomlm` on each data chunk in the 
#' # exact same way.
#' 
#' # proxy for big data feed (`purrr::pmap`)
#' chunks  <- purrr::pmap(mtcars, list)
#' 
#' # initialize the model
#' x <- oomlm(mpg ~ cyl + disp)
#' 
#' # iteratively update model with data chunks
#' for(chunk in chunks) {
#'   update_oomlm(x, chunk)
#' }
#'
#' # Separating model initialization and processing of the first data chunk
#' # enables functional patterns like `reduce` to take the place of loops.
#' # The below example is equivalent to the above `for` loop.
#' 
#' # avoid loops altogether with `purrr::reduce`
#' y <- purrr::reduce(chunks, update_oomlm, .init = oomlm(mpg ~ cyl + disp))
#' 
#' # For maximum flexibility, `ploom` also supports providing data on
#' # initialization similar to [`biglm`](https://github.com/cran/biglm).
#'
#' # initial fit
#' z  <- oomlm(mpg ~ cyl + disp, chunks[[1]])
#' 
#' # iteratively update model with additional data chunks
#' for(chunk in tail(chunks, -1)) {
#'   z <- update_oomlm(x, data = chunk)
#' }
#'
#' @export
oomlm <- function(formula,
                  data     = NULL,
                  weights  = NULL,
                  sandwich = FALSE) {
  
  if(is.null(data)) {
    return(oomlm_formula(formula, weights, sandwich))
  }
  
  oomlm_data(formula, data, weights, sandwich)

}


#' partially initialize oomlm independent of data
#' @keywords internal
oomlm_formula <- function(formula,
                          weights  = NULL,
                          sandwich = FALSE) {

  model_terms <- terms(formula)

  if(!is.null(weights) && !inherits(weights, "formula")) {
    stop("`weights` must be a formula")
  }

  if(sandwich) {
    xy <- list(xy = NULL)
  } else {
    xy <- NULL
  }

  rval <- list(
    call     = sys.call(-1),
    qr       = NULL,
    assign   = NULL,
    terms    = model_terms,
    n        = NULL,
    names    = NULL,
    weights  = weights,
    df.resid = NULL,
    sandwich = xy,
    has_data = FALSE
  )

  class(rval) <- "oomlm"
  rval

}


#' initialize `oomlm` with data, formula is required
#' @keywords internal
oomlm_data <- function(formula,
                       data,
                       weights  = NULL,
                       sandwich = FALSE,
                       ...) {

  obj <- oomlm_formula(formula, weights, sandwich)
  oomlm_init(data, obj)

}


#' complete `oomlm` initialization
#' @keywords internal
oomlm_init <- function(data, obj) {

  model_terms <- obj$terms
  weights     <- obj$weights
  sandwich    <- obj$sandwich

  batch_data  <- model.frame(model_terms, data)

  if(is.null(offset <- model.offset(batch_data))) {
    offset <- 0
  }

  batch_response <- model.response(batch_data) - offset
  batch_data     <- model.matrix(model_terms, batch_data)

  p <- ncol(batch_data)
  n <- nrow(batch_data)

  if(is.null(weights)) {
    batch_weights <- rep(1.0, n)
  } else {
    if(!inherits(weights, "formula")) {
      stop("`weights` must be a formula")
    }
    batch_weights <- model.frame(weights, data)[[1]]
  }

  model_qr <- update(new_bounded_qr(p),
                     batch_data,
                     batch_response,
                     batch_weights)

  obj$call     <- sys.call(-1)
  obj$qr       <- model_qr
  obj$assign   <- attr(batch_data, "assign")
  obj$n        <- n
  obj$names    <- colnames(batch_data)
  obj$weights  <- weights
  obj$df.resid <- n - p
  obj$has_data <- TRUE

  if (!is.null(sandwich)) {
    xx        <- matrix(nrow = n, ncol = p * (p + 1))
    xx[, 1:p] <- batch_data * batch_response
    for (i in 1:p) {
      xx[, p * i + (1:p)] <- batch_data * batch_data[, i]
    }

    sandwich_qr <- update(new_bounded_qr(p * (p + 1)),
                          xx, rep(0, n), batch_weights ^ 2)
    obj$sandwich$xy <- sandwich_qr
  }

  obj

}


#' fit `oomlm` model to new batch of observations
#'
#' @param oomlm model object
#' @param data an optional data frame, list or environment
#'   (or object coercible by as.data.frame to a data frame)
#' @export
update_oomlm <- function(obj, data) {

  if(!obj$has_data) {
    obj <- oomlm_init(data, obj)
    return(obj)
  }

  batch_data  <- model.frame(obj$terms, data)

  if(is.null(offset <- model.offset(batch_data))) {
    offset <- 0
  }

  batch_response <- model.response(batch_data) - offset
  batch_data     <- model.matrix(obj$terms, batch_data)

  if(is.null(obj$weights)) {
    batch_weights <- NULL
  } else {
    batch_weights <- model.frame(obj$weights, data)[[1]]
  }

  if (!identical(obj$assign, attr(batch_data, "assign"))) {
    stop("model matrices incompatible")
  }

  update(obj$qr,
         batch_data,
         batch_response,
         batch_weights)

  obj$n <- obj$n + nrow(batch_data)

  if (!is.null(obj$sandwich)) {

    p <- ncol(batch_data)
    n <- nrow(batch_data)

    xx <- matrix(nrow = n, ncol = p * (p + 1))
    xx[, 1:p] <- batch_data * batch_response
    for (i in 1:p) {
      xx[, p * i + (1:p)] <- batch_data * batch_data[, i]
    }

    obj$sandwich$xy <- update(obj$sandwich$xy,
                              xx, rep(0, n), batch_weights ^ 2)

  }
  obj
}


#' @export
print.oomlm <- function(obj,
                        digits = max(3L, getOption("digits") - 3L),
                        ...) {

  cat("\nOut-of-memory Linear Model:\n",
      paste(deparse(obj$call), sep = "\n", collapse = "\n"),
      "\n\n",
      sep = "")

  beta <- coef(obj)

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
  cat("Observations included: ", obj$n, "\n")

  invisible(obj)

}
