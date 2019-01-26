
#' @importFrom generics tidy
#' @export
generics::tidy

#' @importFrom generics fit
#' @export
generics::fit


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
#' @seealso [oomlm()]
#' @name update
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
#'   stop iterating if model converges before `max_iter` iterations.
#' @param tolerance Tolerance used to determine convergence. Represents
#'  change in coefficient as a multiple of standard error.
#'
#' @return [oomglm()] object after performing `times` IRLS iterations on
#'  `data`.
#' 
#' @seealso [oomglm()]
#' @export
fit.oomglm <- function(object, data, times, tolerance, ...) {
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

