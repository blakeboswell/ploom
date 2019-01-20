
#' @importFrom generics tidy
#' @export
generics::tidy


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
