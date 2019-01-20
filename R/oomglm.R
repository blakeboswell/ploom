
#' Initialize Out of memory Generalized Linear Regression Model
#' 
#' @md
#' @param formula a symbolic description of the model to be fitted of class [formula()].
#' @param family a [family()] object.
#' @param weights a one-sided, single term [formula()] specifying weights. 
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


#' Apply [glm()] transformations to `chunk`
#' 
#' @md
#' @param object [oomglm()] model.
#' @param chunk list created by `unpack_oomchunk()`.
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



#' @export
#' @rdname init_weight
init_weight <- function(object) {
  
  if(object$iter > 0) {
    object$irls$beta <- coef(object)  
  }
  
  object$irls$rss      <- 0.0
  object$irls$deviance <- 0.0
  object$qr   <- NULL
  object$n    <- 0L

  if(!is.null(object$sandwich)) {
    object$sandwich <- list(xy = NULL)
  }
  
  object
 
}


#' @export
#' @rdname init_weight
weight <- function(object, data) {
  
  if(!inherits(data, c("oomdata", "function", "data.frame"))) {
    stop("class of `data` not recognized")
  }
  
  update_oomglm(object, data)
  
}


#' @export
#' @rdname init_weight
end_weight <- function(object, tolerance = 1e-8) {

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


#' Fit [oomglm()] model via Iteratively Reweighted Least Squares (IRLS).
#' 
#' @md
#' @param object [oomglm()] model.
#' @param data [oomdata_tbl()], [oomdata_dbi()], [oomdata_con()],
#'   [tibble()], [data.frame()], or [list()] of observations to fit
#' @param max_iter Maximum number of IRLS iterations to perform. Will 
#'   stop iterating if model converges before `max_iter` iterations.
#' @param tolerance Tolerance used to determine convergence. Represents
#'  change in coefficient as a multiple of standard error.
#'
#' @return [oomglm()] object after performing `max_iter` IRLS iterations on
#'  `data`.
#' 
#' @seealso [oomglm()]
#' @export
iter_weight <- function(object,
                        data,
                        max_iter  = 4L,
                        tolerance = 1e-8) {
  
  
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


#' Out of memory Generalized Linear model
#' 
#' Perform  generalized linear regression using Alan Miller's bounded memory QR
#' factorization algorithm which enables models with `p` variables
#' to be fit in `p^2` memory.
#'   
#' @md
#' @param formula a symbolic description of the model to be fitted of class
#'  [formula()].
#' @param data an optional [oomdata_tbl()], [oomdata_dbi()], [oomdata_con()],
#'  [tibble()], [data.frame()], or [list()] of observations to fit
#' @param family a [family()] object.
#' @param weights a one-sided, single term [formula()] specifying weights.
#' @param start starting values for the parameters in the linear predictor.
#' @param ... ignored.
#' @details
#'  An [oomglm()] model can be in various states of fit depending on the number
#'  of seen observations and rounds of IRLS that have been performed.
#'  It is important to view the model within the context of:
#'  the number of observations processed per round of IRLS (`n`);
#'  the number of IRLS iterations that have been performed (`iter`);
#'  and if the IRLS algorithm has converged (`converged`).
#'
#' @return It is up to the user to know when fitting is complete.
#'  Therefore, only basic model characteristics are provided as values. 
#'  Statistics are available on demand via summary and extractor functions.
#'
#' \item{converged}{Indicates if the IRLS algorithm has converged.}
#' \item{iter}{The number of iterations of IRLS performed.}
#' \item{n}{The number observations processed per round of IRLS.}
#' \item{df.residual}{The residual degrees of freedom.}
#' \item{df.null}{The residual degrees of freedom.}
#' \item{formula}{the [`formula()`] object specifying the linear model.}
#' \item{family}{a [`family()`] object describing the error distribution
#'  and link function used in the model.}
#' \item{terms}{The [`terms()`] object used.}
#' \item{weights}{The weights [`formula()`] provided to the model.}
#' \item{call}{The matched call.}
#' @seealso [iter_weight()], [oomdata_tbl()], [oomdata_dbi()], [oomdata_con()]
#' @aliases predict.oomglm print.oomglm print.summary.oomglm summary.oomglm
#' @export
#' @name oomglm
#' @examples \donttest{
#' # The `oomglm()` function employs Iteratively Weighted Least Squares (IWLS).
#' # The IWLS iterations are performed by the function `iter_weight()` which
#' # makes passes over the data until estimate convergence.
#' 
#' # reweight 4 times or until convergence
#' x <- oomglm(mpg ~ cyl + disp)
#' x <- iter_weight(x, mtcars, max_iter = 4)
#' 
#' tidy(x)
#' 
#' # To fit data in chunks, use `oomdata_tbl()`:
#' 
#' y      <- oomglm(mpg ~ cyl + disp)
#' chunks <- oomdata_tbl(mtcars, chunk_size = 10)
#' y      <- iter_weight(y, chunks, max_iter = 4)
#' 
#' tidy(y)
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


#' Update [oomglm()] object with additional observations
#' 
#' This function is typically called via [iter_weight()] or [weight()].
#' It is exposed for potential non-typical situations.
#' 
#' @param object [oomglm()] model
#' @param data observations to fit
#' @param ... ignored
#' @export
#' @keywords internal
update_oomglm <- function(object, data, ...) {
  
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
    
    trans  <- glm_adjust(object, chunk)
    
    object$qr <- update(qr,
                        chunk$data,
                        trans$z - chunk$offset,
                        trans$w)
    
    if(!is.null(object$sandwich)) {
      object$sandwich$xy <-
        update_sandwich(object$sandwich$xy,
                        chunk$data,
                        chunk$n,
                        chunk$p,
                        trans$z,
                        chunk$offset,
                        trans$w)
    }
    
    intercept <- attr(object$terms, "intercept") > 0L
    
    object$n             <- object$qr$num_obs
    object$df.residual   <- object$n - chunk$p
    object$df.null       <- object$n - as.integer(intercept)
    object$iwls$rss      <- trans$rss
    object$iwls$deviance <- trans$deviance
    
    object
    
  }
  
  if(inherits(data, what = "oomdata")) {
    
    while(!is.null(chunk <- data())) {
      object <- updater(object, chunk, ...)
    }
    
    object
    
  } else {
    updater(object, data, ...)
  }
  
}


#' @method print oomglm
#' @export
print.oomglm <- function(x,
                         digits = max(3L, getOption("digits") - 3L),
                         ...) {
  
  cat("\nCall:  ",
      paste(deparse(x$call), sep = "\n", collapse = "\n"),
      "\n\n", sep = "")
  
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
  
  cat("\nObservations included: ", x$n, "\n")
  cat("Degrees of Freedom:", x$df.null, "Total (i.e. Null); ",
      x$df.residual, "Residual\n")
  
  cat("Residual Deviance:", format(signif(deviance(x), digits)),
      "\tAIC:", format(signif(AIC(x), digits)), "\n\n")
  
  cat("Converged:", x$converged,
      "\nNumber of Fisher Scoring iterations:", x$iter, "\n")
  
  invisible(x)
  
}
