
#' Linear model that uses only `p^2` memory for `p` variables.
#' 
#' @param data an optional data frame, list or environment
#'   (or object coercible by as.data.frame to a data frame)
#' @param formula A model formula: a symbolic description of the
#'   model to be fitted.
#' @param weights A one-sided, single term formula specifying weights
#' @param sandwich TRUE to compute the Huber/White sandwich covariance matrix
#'   (uses `p^4` memory rather than `p^2`)
#' @details The model formula must not contain any data-dependent terms, as
#'   these will not be consistent when updated. Factors are permitted, but 
#'   the levels of the factor must be the same across all data chunks 
#'   (empty factor levels are ok). Offsets are allowed.
#' @export
oomlm <- function(data,
                  formula,
                  weights  = NULL,
                  sandwich = FALSE,
                  ...) {

  model_terms <- terms(formula)
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

  rval <- list(
    call     = sys.call(),
    qr       = model_qr,
    assign   = attr(batch_data, "assign"),
    terms    = model_terms,
    n        = n,
    names    = colnames(batch_data),
    weights  = weights,
    df.resid = n - p
  )

  if (sandwich) {
    xx        <- matrix(nrow = n, ncol = p * (p + 1))
    xx[, 1:p] <- batch_data * batch_response
    for (i in 1:p) {
      xx[, p * i + (1:p)] <- batch_data * batch_data[, i]
    }
    
    sandwich_qr <- update(new_bounded_qr(p * (p + 1)),
                          xx, rep(0, n), batch_weights ^ 2)
    rval$sandwich <- list(xy = sandwich_qr)
  }

  class(rval) <- "oomlm"
  rval

}


#' fit `online_lm` model to new batch of observations
#'
#' @param data an optional data frame, list or environment
#'   (or object coercible by as.data.frame to a data frame)
#' @param object online_lm object
#' @export
update_oomlm <- function(data, obj, ...) {

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


#' mimic base `print.lm` as closely as possible
#' (unofficial reference .. may go away)
#' https://github.com/wch/r-source/blob/trunk/src/library/stats/R/lm.R
#' 
#' @export
print.oomlm <- function(obj,
                        digits = max(3L, getOption("digits") - 3L),
                        ...) {
  
  cat("\nOut-of-memory Linear Model:\n",
      paste(deparse(obj$call), sep = "\n", collapse = "\n"),
      "\n\n",
      sep = "")
  
  betas <- coef(obj)
  
  if(length(betas)) {
    cat("Coefficients:\n")
    print.default(
      format(betas, digits = digits),
      print.gap = 2L,
      quote     = FALSE)
  } else {
    cat("No coefficients\n")
  }
  
  cat("\n")
  cat("Observations included: ", obj$n, "\n")

  invisible(obj)
  
}






