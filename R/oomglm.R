#' @include ploom_shared.R


#' Initialize Updating Generalized Linear Regression Model
#' 
#' @md
#' @noRd
#' @description
#' Performs the details of intializing `oomglm` object called by `oomglm`
#'  function.
#' 
#' @param formula a symbolic description of the model to be fitted of class
#'  `formula`.
#' @param family a `glm` family object.
#' @param weights a one-sided, single term `formula` specifying weights. 
#' @param start starting values for the parameters in the linear predictor.
#' 
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
  
  iwls <- list(
    beta       = start,
    pre_beta   = NULL,
    rss        = 0.0,
    deviance   = 0.0
  )
  
  obj <- list(
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
    iwls          = iwls
  )
  
  class(obj) <- c("oomglm", "oomlm")
  obj
  
}


#' Apply `glm` transformations to `chunk`
#' 
#' @md
#' @noRd
#' @param object `oomglm` model.
#' @param chunk list created by `unpack_oomchunk`.
#' 
#' @keywords internal
glm_adjust <- function(obj, chunk) {

  mm     <- chunk$data
  y      <- chunk$response
  w      <- chunk$weights
  offset <- chunk$offset

  fam    <- obj$family

  beta   <- obj$iwls$beta
  rss    <- obj$iwls$rss
  dev    <- obj$iwls$deviance

  if(is.null(beta)) {
    eta <- rep(0.0, nrow(mm)) + offset
  } else {
    eta <- mm %*% beta + offset
  }

  g       <- fam$linkinv(eta)
  gprime  <- fam$mu.eta(eta)
  z       <- eta + (y - g) / gprime
  fam_var <- fam$variance(g)
  w       <- w * gprime ^ 2 / fam_var

  if(!is.null(beta)) {
    rss <-
      rss + sum((y - g) ^ 2 / (w * fam_var)) * (sum(w) / length(w))
    dev <-
      dev + sum(fam$dev.resids(y, g, w))
  }

  list(
    z = z,
    w = w,
    g = g,
    deviance = dev,
    rss      = rss
  )

}


#' Update `oomglm` from `data.frame`
#' 
#' @md
#' @noRd
#' @param object `oomglm` model.
#' @param data `data.frame` of observations to be fit.
#' 
#' @keywords internal
update_oomglm <- function(obj, data) {

  chunk <- unpack_oomchunk(obj, data)
  
  if(is.null(obj$assign)) {
    obj$assign <- chunk$assign
    obj$names  <- colnames(chunk$data)
  }
  
  if(is.null(obj$qr)) {
    qr <- new_bounded_qr(chunk$p)
  } else {
    qr <- obj$qr
  }
  
  trans  <- glm_adjust(obj, chunk)
  
  obj$qr <- update(qr,
                   chunk$data,
                   trans$z - chunk$offset,
                   trans$w)
  
  intercept <- attr(obj$terms, "intercept") > 0L
  
  obj$n            <- obj$qr$num_obs
  obj$df.residual  <- obj$n - chunk$p
  obj$df.null      <- obj$n - as.integer(intercept)

  obj$iwls$rss      <- trans$rss
  obj$iwls$deviance <- trans$deviance
  
  obj
  
}


#' @keywords internal
iter_update <- function(obj, data) {
  
  while(!is.null(chunk <- data())){
    obj <- update(obj, chunk)
  }
  
  obj
  
}


#' Update `oomglm` with new observations
#' 
#' @md
#' @param obj `oomglm` model.
#' @param data an `oomfeed`, `tibble`, `dataframe`, `list` or `environment`.
#' 
#' @export
update.oomglm <- function(obj, data) {
  
  if(inherits(data, "data.frame")) {
    return(update_oomglm(obj, data))
  }
  
  if(inherits(data, "function")) {
    return(iter_update(obj, data))
  }
  
  stop("class of `data` not recognized")

}


#' Reset model in preparation for new reweight iteration
#' 
#' @md
#' @noRd
#' @param obj `oomglm` model.
#' @keywords internal
init_update <- function(obj) {
  
  obj$iwls$beta     <- coef(obj)
  obj$iwls$rss      <- 0.0
  obj$iwls$deviance <- 0.0
  obj$qr   <- NULL
  obj$n    <- 0L

  obj
 
}


#' Test for IWLS convergence
#' 
#' @md
#' @noRd
#' @param obj `oomglm` model.
#' @keywords internal
end_update <- function(obj, tolerance = 1e-7) {

  obj$iter  <- obj$iter + 1L 
  
  beta_old <- obj$iwls$beta
  
  if(is.null(beta_old)) {
    obj$converged <- FALSE
    return(obj)
  }
  
  beta  <- coef(obj)
  delta <- (beta_old - beta) / sqrt(diag(vcov(obj)))
  obj$converged <- max(abs(delta)) < tolerance
  
  obj
  
} 


#' Reweight `oomglm` model via Iteratively Reweighted Least Squares (IWLS)
#'  method.
#' 
#' @md
#' @param obj `oomglm` model.
#' @param data An `oomfeed`, `tibble`, `dataframe`, `list` or `environment`.
#' @param max_iter Maximum number of IWLS iterations to perform. Will 
#'   stop iterating if model converges before `max_iter` iterations.
#' @param tolerance Tolerance for change in coefficient as a multiple
#'  of standard error.
#'
#' @return `oomglm` object after performing `max_iter` IWLS iterations on
#'  `data`.
#' 
#' @seealso [oomlm()]
#' @examples
#' # proxy for data feed
#' chunks  <- purrr::pmap(mtcars, list)
#' 
#' # initialize the model
#' x <- oomglm(mpg ~ cyl + disp)
#' 
#' # perform iwls
#' x <- reweight(x, mtcars, max_iter = 4)
#' 
#' @export
reweight <- function(obj, data, max_iter, tolerance = 1e-7){
  UseMethod("reweight")
}
setGeneric("reweight")


#' @export
reweight.oomglm <- function(obj,
                            data,
                            max_iter  = 1L,
                            tolerance = 1e-7) {
  
  
  for(i in 1:max_iter) {
    
    if(obj$converged) {
      break
    }
    
    obj <- init_update(obj)
    obj <- update(obj, data)
    obj <- end_update(obj, tolerance)
    
  }
  
  obj
  
}


#' Initialize Updating Generalized Linear model
#' 
#' @md
#' @description
#' Perform  generalized linear regression using Alan Miller's bounded memory QR
#'   factorization algorithm which enables models with `p` variables
#'   to be fit in `p^2` memory.
#' 
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
#' `ooglm` initializes an object of class `ooglm` inheriting from the
#'   class `oomlm`. `ooglm` objects are intended to be iteratively 
#'   updated with new data via calls to [update()]. Iterative fitting
#'   over all data [updates()] are performed with the function [reweight()].
#'   If `data` is provided to the `ooglm` function call, an [update()] round 
#'   will be performed on initialization.
#' 
#'   A `oomglm` object can be in various states of fit depending on the number
#'   of seen observations and rounds of IWLS that have been performed.
#'   It is important to view the model within the context of:
#'   the number of observations processed per round of IWLS (`n`);
#'   the number of IWLS iterations that have been performed (`iter`);
#'   and if the IWLS algorithm has converged (`converged`).
#'
#' @return It is up to the user to know when fitting is complete.
#'   Therefore, only basic model characteristics are provided as values with 
#'   the `ooglm` object. Statistics are available on demand via `summary` and 
#'   extractor functions.
#'
#' \item{converged}{Indicates if the IWLS algorithm has converged.}
#' \item{iter}{The number of iterations of IWLS performed.}
#' \item{n}{The number observations processed per round of IWLS.}
#' \item{df.residual}{The residual degrees of freedom.}
#' \item{df.null}{The residual degrees of freedom.}
#' \item{formula}{the [stats::formula()] object specifying the linear model.}
#' \item{family}{a [stats::family()] object describing the error distribution
#'   and link function used in the model.}
#' \item{terms}{The [stats::terms()] object used.}
#' \item{weights}{The weights `formula` provided to the model.}
#' \item{call}{The matched call.}
#' 
#' @examples
#' # proxy for data feed
#' chunks  <- purrr::pmap(mtcars, list)
#' 
#' # initialize the model
#' x <- oomglm(mpg ~ cyl + disp)
#' 
#' # perform iwls
#' x <- reweight(x, mtcars, max_iter = 4)
#'
#' @export
oomglm <- function(formula,
                   data     = NULL,
                   family   = gaussian(),
                   weights  = NULL,
                   start    = NULL,
                   ...) {

  obj <- init_oomglm(formula,
                     family,
                     weights,
                     start)

  if(!is.null(data)) {
    obj <- update(obj, data)
  }

  obj

}


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
