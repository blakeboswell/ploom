 
#' @importFrom generics tidy
#' @export
generics::tidy

#' @importFrom generics fit
#' @export
generics::fit

#' @importFrom generics glance
#' @export
generics::glance

#' @importFrom generics augment
#' @export
generics::augment


broom_names <- function(col_names) {
  
}


#' Fit `oomlm()` model to additonal observations
#' 
#' @md
#' @description
#' Update ploom model fit with new data. 
#' 
#' @param object [oomlm()] model to be updated
#' @param data an optional [oomdata_tbl()], [oomdata_dbi()], [oomdata_con()],
#'   [tibble()], [data.frame()], or [list()] of observations to fit
#' @param ... ignored
#' 
#' @method fit oomlm
#' @seealso [oomlm()]
#' @export
#' @rdname fit.oomlm
fit.oomlm <- function(object, data, ...) {
  update(object, data)
}


#' Fit [oomglm()] model via Iteratively Reweighted Least Squares (IRLS).
#' 
#' @md
#' @param object [oomglm()] model.
#' @param data [oomdata_tbl()], [oomdata_dbi()], [oomdata_con()],
#'   [tibble()], [data.frame()], or [list()] of observations to fit
#' @param times Maximum number of IRLS iterations to perform. Will 
#'   stop iterating if model converges before `times` iterations.
#' @param tolerance Tolerance used to determine convergence. Represents
#'  change in coefficient as a multiple of standard error.
#' @param ... ignored
#' 
#' @return [oomglm()] object after performing `times` IRLS iterations on
#'  `data`.
#' 
#' @method fit oomglm
#' @seealso [oomglm()]
#' @export
#' @rdname fit.oomglm
fit.oomglm <- function(object, data, times = 2L, tolerance = 1e-8, ...) {
  iter_weight(object, data, times, tolerance)
}


#' Tidy an oomlm model
#' 
#' @return A data.frame with columns for coefficient names, estimates, standard
#' errors, confidence intervals, p-values, degrees of freedom, and the
#' name of the outcome variable
#'
#' @param x [oomlm()] model
#' @param ... ignored
#' 
#' @method tidy oomlm
#' @export
#' @md
tidy.oomlm <- function(x, ...) {
  
  coef_df  <- tibble::rownames_to_column(
    data.frame(
      cbind(summary(x)$coefficients), 
      confint(x)
    ),
    var = "term"
  )
  
  names(coef_df) <- c(
    "term", 
    "estimate",
    "std.error",
    "statistic",
    "p.value",
    "conf.low",
    "conf.high"
  )
  
  tibble::as_tibble(coef_df)
  
}


#' Returns a tibble with exactly one row of goodness of fitness measures
#' and related statistics.
#' 
#' @param x `oomlm` model object
#' @param ... ignored.
#'
#' @return A one-row [tibble::tibble] with columns:
#' 
#'   \item{r.squared}{The percent of variance explained by the model}
#'   \item{adj.r.squared}{r.squared adjusted based on the degrees of freedom}
#'   \item{sigma}{The square root of the estimated residual variance}
#'   \item{statistic}{F-statistic}
#'   \item{p.value}{p-value from the F test, describing whether the full
#'   regression is significant}
#'   \item{df}{Degrees of freedom used by the coefficients}
#'   \item{logLik}{the data's log-likelihood under the model}
#'   \item{AIC}{the Akaike Information Criterion}
#'   \item{BIC}{the Bayesian Information Criterion}
#'   \item{deviance}{deviance}
#'   \item{df.residual}{residual degrees of freedom}
#'
#' @method glance oomlm
#' @export
#' @seealso [glance()]
glance.oomlm <- function(x, ...) {
  sx   <- summary.oomlm(x)
  rval <- glance.summary.oomlm(sx, ...)
  rval$logLik <- logLik(x)
  rval$AIC    <- AIC(x)
  rval$BIC    <- BIC(x)
  rval$deviance    <- deviance(x)
  rval$df.residual <- x$df.residual
  rval
}


#' @method glance summary.oomlm
#' @rdname glance.oomlm
#' @export
glance.summary.oomlm <- function(x, ...) {
  
  fstat <- x$fstatistic
  
  tibble::tibble(
    r.squared       = x$r.squared
    , adj.r.squared = x$adj.r.squared
    , sigma         = x$sigma
    , statistic     = x$fstatistic[1]
    , p.value       = pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE)
  )
  
}


#' @method glance oomglm
#' @rdname glance.oomlm
#' @export
glance.oomglm <- function(x, ...) {
  tibble::tibble(
    df.null  = x$df.null
    , logLik = logLik(x)
    , AIC    = AIC(x)
    , BIC    = BIC(x)
    , deviance    = deviance(x)
    , df.residual = x$df.residual
  )
}


#' augment data with prediction, std error, and residuals
#' 
#' @param x `oomlm()` model
#' @param data `tibble()` or other data source
#' @param std_error calculate standard error of prediction
#' @param interval interval type to return
#' @param ... ignored
#' 
#' @method augment oomlm
#' @export
#' @seealso [augment()], [stats::predict.lm()]
augment.oomlm <- function(x, data,
                          std_error = FALSE,
                          interval  = "confidence",
                          ...) {
  
  df        <- model_frame_tibble(x, data)
  names(df) <- gsub("([._])|[[:punct:]]", ".", names(df))
  
  chunk <- unpack_oomchunk(x, data)
  u <- residuals_oomlm_x(x, chunk)
  y <- predict_oomlm_x(x, chunk, std_error = std_error, interval = interval)

  if(std_error) {
    df[[".fitted"]]    <- y$fit[, 1]
    df[[".std_error"]] <- y$std_error
    df[[".resid"]]     <- u[, 1]
    if(!is.null(interval)) {
      df[[".pred_lower"]] <- y$fit[, 2]
      df[[".pred_upper"]] <- y$fit[, 3]
    }
  } else {
    df[[".fitted"]] <- y[, 1]
    df[[".resid"]]  <- u[, 1]
  }

  df
  
}


#' augment data with prediction, std error, and residuals
#' 
#' @param x `oomlm()` model
#' @param data `tibble()` or other data source
#' @param type link or response
#' @param std_error calculate standard error of prediction
#' @param ... ignored
#' 
#' @method augment oomglm
#' @export
#' @seealso [augment()], [stats::predict.lm()]
augment.oomglm <- function(x, data,
                           type = "response",
                           std_error = FALSE,
                           ...) {

  df        <- model_frame_tibble(x, data)
  names(df) <- gsub("([._])|[[:punct:]]", ".", names(df))
  
  chunk <- unpack_oomchunk(x, data)
  u     <- residuals_oomglm_x(x, chunk, type)
  y     <- predict_oomglm_x(x, chunk, type, std_error)
  
  if(std_error) {
    df[[".fitted"]]    <- y$fit[, 1]
    df[[".std_error"]] <- y$std_error
    df[[".resid"]]     <- u[, 1]
  } else {
    df[[".fitted"]] <- y[, 1]
    df[[".resid"]]  <- u[, 1]
  }

  df
  
}

