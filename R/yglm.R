#' @include yotta_shared.R


#' Initialize Updating Generalized Linear Regression Model
#' 
#' @md
#' @noRd
#' @description
#' Performs the details of intializing `yglm` object called by
#' `yglm` function.
#' 
#' @param formula a symbolic description of the model to be fitted of class `formula`.
#' @param family a `glm` family object.
#' @param weights a one-sided, single term `formula` specifying weights. 
#' @param start starting values for the parameters in the linear predictor.
#' 
#' @keywords internal
init_yglm <- function(formula,
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
    rss        = 0.0,
    deviance   = 0.0
  )
  
  obj <- list(
    call          = sys.call(-1),
    qr            = NULL,
    assign        = NULL,
    terms         = terms(formula),
    n             = 0,
    names         = NULL,
    df.residual   = NULL,
    df.null       = NULL,
    family        = family,
    iwls          = iwls,
    converged     = FALSE,
    iter          = 0L,
    weights       = weights,
    pweights      = 0,
    zero_weights  = 0
  )
  
  class(obj) <- c('yglm', 'ylm')
  obj
  
}


#' Apply `glm` transformations to `chunk`
#' 
#' @md
#' @noRd
#' @param object `yglm` model.
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


#' Update `yglm` from `data.frame`
#' 
#' @md
#' @noRd
#' @param object `yglm` model.
#' @param data `data.frame` of observations to be fit.
#' 
#' @keywords internal
update_yglm_data <- function(obj, data) {

  chunk <- unpack_oomchunk(obj, data)
  
  if(is.null(obj$assign)) {
    obj$assign <- chunk$assign
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
  
  zero_wts  <- trans$w == 0
  intercept <- attr(obj$terms, "intercept") > 0L
  
  obj$n            <- obj$n + chunk$n - sum(zero_wts)
  obj$names        <- colnames(chunk$data)
  obj$df.residual  <- obj$n - chunk$p
  obj$df.null      <- obj$n - as.integer(intercept)
  obj$pweights     <- obj$pweights + sum(log(trans$w[!zero_wts]))
  obj$zero_weights <- obj$zero_weights + sum(zero_wts)
  
  obj$iwls$rss      <- trans$rss
  obj$iwls$deviance <- trans$deviance
  
  obj
  
}


#' Update `yglm` given `function` that returns `data.frame`s
#'   when iterated over
#' 
#' @md
#' @noRd
#' @param obj `yglm` model
#' @param data `function`
#' 
#' @keywords internal
update_yglm_function <- function(obj, data) {
  
  while(!is.null(chunk <- data())){
    obj <- update_yglm(obj, chunk)
  }
  
  obj
  
}


#' Update `yglm` with new observations
#' 
#' @md
#' @param obj `yglm` model.
#' @param data an `oomfeed`, `tibble`, `dataframe`, `list` or `environment`.
#' 
#' @export
update.yglm <- function(obj, data) {
  
  if(inherits(data, "data.frame")) {
    return(update_yglm_data(obj, data))
  }
  
  if(inherits(data, "function")) {
    return(update_yglm_function(obj, data))
  }
  
  stop("class of `data` not recognized")
  
}


#' Reset model in preparation for new reweight iteration
#' 
#' @md
#' @noRd
#' @param obj `ygml` model.
#' @param beta_old `ygml` coefficients resulting from previous
#'   reweight iteration.
#' @keywords internal
reset_yglm <- function(obj, beta_old) {
  
  obj$iwls$rss      <- 0.0
  obj$iwls$deviance <- 0.0
  obj$qr   <- NULL
  obj$n    <- 0L

  obj
 
}



#' Reweight `yglm` model via Iteratively Reweighted Least Squares (IWLS) method
#' 
#' @md
#' @param obj `yglm` model.
#' @param data An `oomfeed`, `tibble`, `dataframe`, `list` or `environment`.
#' @param num_iter Number of IWLS iterations to perform. Will automatically
#'   stop iterating if model converges before `num_iter` iterations.
#' @param tolerance Tolerance for change in coefficient (as multiple of standard error).
#'
#' @return `yglm` object after performing `num_iter` IWLS iterations on `data`
#' 
#' @seealso [ylm()]
#' @examples
#' # proxy for data feed
#' chunks  <- purrr::pmap(mtcars, list)
#' 
#' # initialize the model
#' x <- ylm(mpg ~ cyl + disp)
#' 
#' # perform iwls
#' x <- reweight(x, mtcars, num_iter = 4)
#' 
#' @export
reweight <- function(obj, data, num_iter, tolerance = 1e-7) { UseMethod("reweight") }
setGeneric("reweight")


#' @export
reweight.yglm <- function(obj,
                          data,
                          num_iter  = 1L,
                          tolerance = 1e-7) {
  
  if(obj$converged) {
    return(obj)
  }
  
  for(i in 1:num_iter) {
    
    beta_old <- coef(obj)
    obj      <- reset_yglm(obj)
    
    obj            <- update(obj, data)
    obj$iwls$beta  <- coef(obj)
    obj$iter <- obj$iter + 1L
    
    if(!is.null(beta_old)) {
      delta <- (beta_old - obj$iwls$beta) / sqrt(diag(vcov(obj)))
      if (max(abs(delta)) < tolerance){
        obj$converged <- TRUE
        break
      }
    }

  }
  
  obj
  
}


#' Initialize Updating Generalized Linear Regression model
#' 
#' @md
#' @description
#' Perform  generalized linear regression usig Alan Miller's bounded memory QR
#'   factorization algorithm which enables models with `p` variables
#'   to be fit in `p^2` memory.
#' 
#' @param formula a symbolic description of the model to be fitted of class `formula`.
#' @param data an optional `oomfeed`, `tibble`, `dataframe`, `list` or `environment`.
#' @param family A `glm` family object.
#' @param weights a one-sided, single term `formula` specifying weights.
#' @param start starting values for the parameters in the linear predictor.
#' @param ... ignored.
#' 
#' @details
#' A `ylgm` object can be in various states of fit depending on the number of seen 
#'   observations and rounds of IWLS that have been performed.  Therefore, it is important
#'   to consider all return values within the following context
#'   
#'   * The number of observations processed per round of IWLS (`n`).
#'   * The number of IWLS iterations that have been performed (`iter`).
#'   * If the IWLS algorithm has converged (`converged`).
#'
#' @return `yglm` returns an object of class `yglm` inheriting from the class `ylm`.
#'
#' \item{coefficients}{A named vector of coefficients.}
#' \item{rank}{The numeric rank of the linear model}
#' \item{family}{a [stats::family()] object describing the error distribution and
#'   link function used in the model.}
#' \item{n}{The number observations processed per round of IWLS.}
#' \item{iter}{The number of iterations of IWLS performed.}
#' \item{df.residual}{The residual degrees of freedom.}
#' \item{converged}{Indicates if the IWLS algorithm has converged.}
#' \item{call}{The matched call.}
#' \item{terms}{The [stats::terms()] object used.}
#' \item{qr}{A [yotta::BoundedQr()] object resulting from the latest round of IWLS.}
#' \item{weights}{The weights `formula` provided to the model.}
#' 
#' @examples
#' # proxy for data feed
#' chunks  <- purrr::pmap(mtcars, list)
#' 
#' # initialize the model
#' x <- ylm(mpg ~ cyl + disp)
#' 
#' # perform iwls
#' x <- reweight(x, mtcars, num_iter = 4)
#'
#' @export
yglm <- function(formula,
                 data     = NULL,
                 family   = gaussian(),
                 weights  = NULL,
                 start    = NULL,
                 ...) {

  obj <- init_yglm(formula,
                   family,
                   weights,
                   start)

  if(!is.null(data)) {
    obj <- update_yglm(obj, data)
  }

  obj

}


#' @export
print.yglm <- function(x,
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
  
  # if(nzchar(mess <- naprint(x$na.action))) cat("  (",mess, ")\n", sep = "")
  
  cat("Residual Deviance:", format(signif(deviance(x), digits)),
      "\tAIC:", format(signif(AIC(x), digits)))
  
  cat("\n")
  invisible(x)
  
}
