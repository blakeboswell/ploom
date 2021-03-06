# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#'  Algorithm AS 274: Least Squares Routines to Supplement Those of Gentleman
#'  Author(s): Alan J. Miller
#'  Source: Journal of the Royal Statistical Society.
#'  Series Applied Statistics, Vol. 41, No. 2
#'  (1992), pp. 458-478
#'
#'  num_obs_
#'  the latest count of observations processed. double precision is
#'  used so that it will be numeric on the R side accomodating row
#'  counts larger than R integers can contain
#'  
#'  num_params_ 
#'  the total number of dependent variables,
#'  including the constant if present
#'
#'  rbar_dim_
#'  the dimension of the upper triangular matrix rbar_
#'  
#'  tol_set_
#'  a logical variable which is set when subroutine tolset() has
#'  been called to calculate tolerances for use in testing for
#'  singularities.
#'  
#'  rss_set_
#'  a logical variable indicating whether residual sums of squares
#'  are available.
#'  
#'  D_
#'  array of row multipliers for the Cholesky factorization.
#'  
#'  thetab_
#'  vector of projections (after scaling by sqrt(D))
#'
#'  rbar_
#'  upper-triangular matrix excluding the implicit 1's on the diagonal,
#'  representing the Cholesky factorization.
#'
#'  tol_
#'  array of tolerances used in testing for singularities
#'  
#'  rss_
#'  array of residual sums of squares
#'
#'  sserr_
#'  residual sum of squares with all of the variables included
#'  equal to last element in rss_
#'  
#' @keywords internal
#' @name BoundedQr
NULL

