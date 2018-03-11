
#' Linear model that uses only `p^2` memory for `p` variables.
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
#' # Like base `lm`, `oomlm` accepts any data object coercible
#' # to data.frame
#' x <- oomlm(mtcars[1:15, ], mpg ~ cyl + disp + hp + wt)
#' 
#' # In-memory data can be passed to `oomlm` in chunks:
#' x <- update_oomlm(mtcars[16:nrow(mtcars), ], x)
#' summary(x)
#' 
#' # Updates to `oomlm` objects happen as side effects, so assignment from
#' # `update_oomlm` is not required
#' x <- oomlm(mtcars[1, ], mpg ~ cycl + disp + hp + wt)
#' purrr::walk(2:nrow(mtcars), ~ update_oomlm(mtcars[., ], x))
#' summary(x)
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


#' initilize empty `oomlm`` without data
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
    call     = sys.call(),
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


#' initialize `oomlm` with data
#' @keywords internal
oomlm_data <- function(formula,
                       data,
                       weights  = NULL,
                       sandwich = FALSE,
                       ...) {

  obj <- oomlm_formula(formula, weights, sandwich)
  oomlm_init(data, obj)

}


#' data and formula are known, init a "complete" `oomlm` object
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

  obj$call     <- sys.call()
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


#' #' @export
#' update_oomlm <- function(x, ...) {
#'   UseMethod("update_oomlm")
#' }
#' 
#' 
#' #' @export
#' update_oomlm.default <- function(x, ...) {
#'   chunk_include(data = x, ...)
#' }
#' 
#' 
#' #' @export
#' update_oomlm.oomlm <- function(x, ...) {
#'   chunk_include(..., obj = x)
#' }


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
    sandwich_qr <- update(obj$sandwich$xy,
                          xx, rep(0, n), batch_weights ^ 2)
    obj$sandwich <- list(xy = sandwich_qr)

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

