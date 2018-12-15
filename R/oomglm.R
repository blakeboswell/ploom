
#' Initialize Updating Generalized Linear Regression Model
#' 
#' Performs the details of intializing `oomglm` object called by `oomglm()`
#'  function.
#' 
#' @md
#' @param formula a symbolic description of the model to be fitted of class
#'  `formula`.
#' @param family a `glm` family object.
#' @param weights a one-sided, single term `formula` specifying weights. 
#' @param start starting values for the parameters in the linear predictor.
#' @keywords internal
init_oomglm <- function(formula,
                        family,
                        weights,
                        start) {
  
  if(is.character(family)) {
    family <- get(family, mode = "function", envir = parent.frame())
  }
  if(is.function(family)) {
    family <- family()
  }
  if(is.null(family$family)) {
    print(family)
    stop("'family' not recognized")
  }
  
  if(!is.null(weights) && !inherits(weights, "formula")) {
    stop("`weights` must be a formula")
  }
  
  irls <- list(
    beta       = start,
    pre_beta   = NULL,
    rss        = 0.0,
    deviance   = 0.0
  )
  
  object <- list(
    converged     = FALSE,
    iter          = 0,
    n             = 0,
    df.residual   = NULL,
    df.null       = NULL,
    formula       = formula,
    family        = family,
    terms         = terms(formula),
    weights       = weights,
    call          = sys.call(-1),
    qr            = NULL,
    names         = NULL,
    assign        = NULL,
    irls          = irls
  )
  
  class(object) <- c("oomglm", "oomlm")
  object
  
}


#' Apply `glm` transformations to `chunk`
#' 
#' @md
#' @param object `oomglm` model.
#' @param chunk list created by `unpack_oomchunk`.
#' @keywords internal
glm_adjust <- function(object, chunk) {

  mm     <- chunk$data
  y      <- chunk$response
  w      <- chunk$weights
  offset <- chunk$offset

  fam    <- object$family

  beta   <- object$irls$beta
  rss    <- object$irls$rss
  dev    <- object$irls$deviance

  if(is.null(beta)) {
    eta <- rep(0.0, nrow(mm)) + offset
  } else {
    eta <- mm %*% beta + offset
  }

  g       <- fam$linkinv(eta)
  gprime  <- fam$mu.eta(eta)
  z       <- eta + (y - g) / gprime
  fam_var <- fam$variance(g)
  ww      <- w * gprime ^ 2 / fam_var

  if(!is.null(beta)) {
    rss <-
      rss + sum((y - g) ^ 2 / (w * fam_var)) * (sum(w) / length(w))
    dev <-
      dev + sum(fam$dev.resids(y, g, w))
  }

  list(
    z = z,
    w = ww,
    g = g,
    deviance = dev,
    rss      = rss
  )

}


#' Prepare model for IRLS iteration
#' 
#' Reset variables used for IRLS calculation
#' 
#' @md
#' @param object `oomglm` model.
#' 
#' @export
init_weight <- function(object) {
  
  if(object$iter > 0) {
    object$irls$beta <- coef(object)  
  }
  
  object$irls$rss      <- 0.0
  object$irls$deviance <- 0.0
  object$qr   <- NULL
  object$n    <- 0L

  object
 
}


#' Fit weighted least squares as part of IRLS iteration
#' 
#' @md
#' @param object `oomglm` model.
#' @param data `data.frame` of observations to be fit.
#' @export
weight <- function(object, data) {
  
  if(!inherits(data, c("function", "data.frame"))) {
    stop("class of `data` not recognized")
  }
  
  if(inherits(data, "data.frame")) {
    data <- oomfeed(data, chunk_size = nrow(data))
  }
  
  while(!is.null(chunk <- data())){
    object <- update(object, chunk)
  }
  
  object
  
}


#' Complete model for IRLS iteration
#' 
#' Update variables used for IRLS calculation, increment `iter`, and
#' determine if model has converged
#' 
#' @md
#' @param object `oomglm` model.
#' @param tolerance Tolerance for change in coefficient as a multiple
#'  of standard error.
#'
#' @export
end_weight <- function(object, tolerance = 1e-7) {

  object$iter <- object$iter + 1L 
  beta_old    <- object$irls$beta
  
  if(is.null(beta_old)) {
    return(object)
  }
  
  beta  <- coef(object)
  delta <- (beta_old - beta) / sqrt(diag(vcov(object)))
  object$converged <- max(abs(delta)) < tolerance
  
  object
  
} 


#' Fit `oomglm` model via Iteratively Reweighted Least Squares (IRLS).
#' 
#' @md
#' @param object `oomglm` model.
#' @param data An `oomfeed`, `tibble`, `dataframe`, `list` or `environment`.
#' @param max_iter Maximum number of IRLS iterations to perform. Will 
#'   stop iterating if model converges before `max_iter` iterations.
#' @param tolerance Tolerance for change in coefficient as a multiple
#'  of standard error.
#'
#' @return `oomglm` object after performing `max_iter` IRLS iterations on
#'  `data`.
#' 
#' @seealso [`oomglm()`]
#' @export
iter_weight <- function(object,
                        data,
                        max_iter  = 1L,
                        tolerance = 1e-7) {
  
  
  for(i in 1:max_iter) {
    
    if(object$converged) {
      break
    }
    
    object <- init_weight(object)
    object <- weight(object, data)
    object <- end_weight(object, tolerance)
    
  }
  
  object
  
}


#' Initialize Updating Generalized Linear model
#' 
#' Perform  generalized linear regression using Alan Miller's bounded memory QR
#'   factorization algorithm which enables models with `p` variables
#'   to be fit in `p^2` memory.
#'   
#' @md
#' @param formula a symbolic description of the model to be fitted of class
#'   `formula`.
#' @param data an optional `oomfeed`, `tibble`, `dataframe`, `list` or
#'   `environment`.
#' @param family A `glm` family object.
#' @param weights a one-sided, single term `formula` specifying weights.
#' @param start starting values for the parameters in the linear predictor.
#' @param ... ignored.
#' 
#' @details
#' `oomglm` initializes an object of class `oomglm` inheriting from the
#'   class `oomlm`. `oomglm` objects are intended to be iteratively 
#'   updated with new data via calls to [update()]. Iterative fitting
#'   over all data updates are performed with the function [iter_weight()].
#'   If `data` is provided to the `oomglm()` function call, an `update()` round 
#'   will be performed on initialization.
#' 
#'   A `oomglm` object can be in various states of fit depending on the number
#'   of seen observations and rounds of IRLS that have been performed.
#'   It is important to view the model within the context of:
#'   the number of observations processed per round of IRLS (`n`);
#'   the number of IRLS iterations that have been performed (`iter`);
#'   and if the IRLS algorithm has converged (`converged`).
#'
#' @return It is up to the user to know when fitting is complete.
#'   Therefore, only basic model characteristics are provided as values with 
#'   the `oomglm` object. Statistics are available on demand via `summary` and 
#'   extractor functions.
#'
#' \item{converged}{Indicates if the IRLS algorithm has converged.}
#' \item{iter}{The number of iterations of IRLS performed.}
#' \item{n}{The number observations processed per round of IRLS.}
#' \item{df.residual}{The residual degrees of freedom.}
#' \item{df.null}{The residual degrees of freedom.}
#' \item{formula}{the [`stats::formula()`] object specifying the linear model.}
#' \item{family}{a [`stats::family()`] object describing the error distribution
#'   and link function used in the model.}
#' \item{terms}{The [`stats::terms()`] object used.}
#' \item{weights}{The weights [`stats::formula()`] provided to the model.}
#' \item{call}{The matched call.}
#' @seealso [oomlm()]
#' @aliases predict.oomglm print.oomglm print.summary.oomglm
#'   summary.oomglm
#' @export
#' @name oomglm
#' @examples \donttest{
#' # The `oomglm()` function fits generalized linear models via 
#' # Iteratively Weighted Least Squares (IWLS).  
#'
#' # When fitting in-memory data the process is similar to `oomlm()` but we use the 
#' # function `iter_weight()` instead of `update()`. `iter_weight()` fits the model
#' # via iterative passes over the data until convergence.
#' 
#' # initialize the model
#' x <- oomglm(mpg ~ cyl + disp)
#' 
#' # re-weight 8 times or until convergence
#' x <- iter_weight(x, mtcars, max_iter = 8)
#' 
#' # To fit data in chunks, use `oomfeed()`:
#' 
#' # initialize the model
#' x    <- oomglm(mpg ~ cyl + disp)
#' feed <- oomfeed(mtcars, chunk_size = 10)
#' 
#' # iteratively reweight model
#' x <- iter_weight(x, feed, max_iter = 8)
#' 
#' # The `iter_weight()` process can also be implemented directly with
#' # component `ploom` functions:
#' 
#' x    <- oomglm(mpg ~ cyl + disp)
#' feed <- oomfeed(mtcars, chunk_size = 10)
#' 
#' # a first pass over the data
#' x <- init_weight(x)
#' x <- weight(x, feed)
#' x <- end_weight(x)
#' x
#' 
#' # a second pass over the data
#' x <- init_weight(x)
#' x <- weight(x, feed)
#' x <- end_weight(x)
#' x
#' 
#' # This is meant to be useful when debugging / evaluating models with long 
#' # runtimes by exposing the individual steps of the model process for inspection. 
#'
#' }
oomglm <- function(formula,
                   data     = NULL,
                   family   = gaussian(),
                   weights  = NULL,
                   start    = NULL,
                   ...) {

  object <- init_oomglm(formula,
                        family,
                        weights,
                        start)

  if(!is.null(data)) {
    object <- weight(object, data)
  }

  object

}


#' @export
#' @rdname update
update.oomglm <- function(object, data, ...) {
  
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
  
  trans  <- glm_adjust(object, chunk)
  
  object$qr <- update(qr,
                      chunk$data,
                      trans$z - chunk$offset,
                      trans$w)
  
  intercept <- attr(object$terms, "intercept") > 0L
  
  object$n             <- object$qr$num_obs
  object$df.residual   <- object$n - chunk$p
  object$df.null       <- object$n - as.integer(intercept)
  object$iwls$rss      <- trans$rss
  object$iwls$deviance <- trans$deviance
  
  object
  
}


#' @method print oomglm
#' @export
print.oomglm <- function(x,
                         digits = max(3L, getOption("digits") - 3L),
                         ...) {
  
  cat("\nCall:  ",
      paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")
  
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
  
  cat("\nObservations included: ", x$n, "\n")
  cat("Degrees of Freedom:", x$df.null, "Total (i.e. Null); ",
      x$df.residual, "Residual\n")
  
  cat("Residual Deviance:", format(signif(deviance(x), digits)),
      "\tAIC:", format(signif(AIC(x), digits)), "\n\n")
  
  cat("Converged:", x$converged,
      "\nNumber of Fisher Scoring iterations:", x$iter, "\n")
  
  invisible(x)
  
}
