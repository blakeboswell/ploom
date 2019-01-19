

#' Initialize Updating Linear Model
#' 
#' @md
#' @param formula a symbolic description of the model to be fitted
#'  of class `formula`.
#' @param weights A one-sided, single term `formula` specifying weights.
#' @keywords internal
init_oomlm <- function(formula, weights = NULL) {
  
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


#' Out of memory Linear model
#' 
#' Perform linear regression via Alan Miller's bounded memory QR
#' factorization algorithm which enables models with `p` variables
#' to be fit in `p^2` memory.
#' 
#' @md
#' @param formula a symbolic description of the model to be fitted of class
#'  [formula()].
#' @param data an optional [oomdata_tbl()], [oomdata_dbi()], [oomdata_con()],
#'  [tibble()], [data.frame()], or [list()] of observations to fit
#' @param weights a one-sided, single term [formula()] specifying weights.
#' @param ... ignored.
#' @details
#'  The provided [formula()] must not contain any data-dependent terms to ensure
#'  consistency across calls to [update()]. Factors are permitted, but the
#'  levels of the factor must be the same across all data chunks. Empty factor
#'  levels are accepted.
#'
#' @return A [oomlm()] model is perpetually in an _in-progress_ state. It is up
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
#' @seealso [oomglm()], [oomdata_tbl()]
#' @aliases AIC.oomlm coef.oomlm confint.oomlm deviance.oomlm family.oomlm 
#'  formula.oomlm predict.oomlm print.oomlm print.summary.oomlm summary.oomlm 
#'  vcov.oomlm
#' @export
#' @name oomlm
#' @examples \donttest{
#' # `oomglm()` is similar to `lm()` for fitting in memory data
#' 
#' x <- oomlm(mpg ~ cyl + disp, mtcars)
#' print(x)
#' 
#' 
#' # For iteratively updating models, initalize with a call to `oomlm()` and
#' # update with the `update()` function
#' 
#' chunks <- purrr::pmap(mtcars, list)
#' 
#' y <- oomlm(mpg ~ cyl + disp)
#' 
#' for(chunk in chunks) {
#'   y <- update(y, chunk)
#' }
#' 
#' tidy(x)
#' 
#' # `oomdata_tbl()` facilitates iterating through data rows in chunks
#' chunks  <- oomdata_tbl(mtcars, chunk_size = 1)
#' 
#' # `oomlm()` will automatically fit to all chunks in an `oomdata()` functions
#' z <- oomlm(mpg ~ cyl + disp, data = chunks)
#' 
#' summary(z)
#'
#' }
oomlm <- function(formula, data = NULL, weights  = NULL, ...) {
  
  object <- init_oomlm(formula, weights)
  
  if(!is.null(data)) {
    object <- update(object, data)
  }
  
  object
  
}


#' @export
#' @rdname update
update.oomlm <- function(object, data, ...) {
  
  updater <- function(object, data, ...) {
    
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
    
    if(!is.null(object$sandwich)) {
      object$sandwich$xy <-
        update_sandwich(object$sandwich$xy,
                        chunk$data,
                        chunk$n,
                        chunk$p,
                        chunk$response,
                        chunk$offset,
                        chunk$weights)
    }
    
    object$n            <- object$qr$num_obs
    object$df.residual  <- object$n - chunk$p
    
    object
    
  }
  
  if(inherits(data, what = c("oomdata_tbl", "oomdata_dbi", "oomdata_con"))) {
    
    while(!is.null(chunk <- data())) {
      object <- updater(object, chunk, ...)
    }
    
    object
  
  } else {
    updater(object, data, ...)
  }
  
}



#' @export
#' @method print oomlm
print.oomlm <- function(x,
                        digits = max(3L, getOption("digits") - 3L),
                        ...) {
  
  cat("\nCall:  ",
      paste(deparse(x$call), sep = "\n", collapse = "\n"),
      "\n\n",
      sep = "")
  
  if(!is.null(x$se_type)) {
    cat(paste("Standard error type:", x$se_type), "\n\n")  
  }
  
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

