

#' Class for out of memory QR factorization
#' 
#' @details 
#  Algorithm AS 274: Least Squares Routines to Supplement Those of Gentleman
#'  Author(s): Alan J. Miller
#'  Source: Journal of the Royal Statistical Society.
#'  Series Applied Statistics, Vol. 41, No. 2
#'  (1992), pp. 458-478
#'
#' @keywords internal
#' @docType class
#' @name Rcpp_BoundedQr-class
#' @aliases Rcpp_BoundedQr-class
NULL


#' initialize new `BoundedQr` object
#'
#' @param np int number of independent parameters in model
#'   including the intercept when model has intercept
#' @export
#' @keywords internal
new_bounded_qr <- function(np) {
  
  if(np < 1) {
    stop("Number of parameters must be greater than 0")
  }
  
  methods::new(Class = BoundedQr, np)
}


#' wrapper for `BoundedQr` method `update`
#'
#' @param object BoundedQr object to update
#' @param X numeric matrix of covariate observations
#' @param y numeric vector of response
#' @param weights numeric vector of observation weights
#' @param ... ignored
#' @export
#' @keywords internal
update.Rcpp_BoundedQr <- function(object, X, y, weights, ...) {
  
  if(ncol(X) != object$num_params) {
    stop("Invalid column dimension for `X`")
  }
  
  if (length(weights) == 0) {
    weights <- rep(1.0, length(y))
  }
  
  if(!all.equal.numeric(nrow(X),
                        length(y),
                        length(weights))) {
    stop("Incompatible row dimensions for `X`, `y`, `weights`")
  }
  
  object$update(X, y, weights)
  object
}


#' wrapper for `BoundedQr` method `betas`
#' 
#' @param object BoundedQr object
#' @param nvar int number of coefficients to return
#' @param ... ignored
#' @export
#' @keywords internal
coef.Rcpp_BoundedQr <- function(object, nvar = NULL, ...){
  
  if (is.null(nvar)) {
    nvar <- object$num_params
  }
    
  if (nvar < 1 | nvar > object$num_params) {
    stop("Invalid value of `nvar`")
  }
  
  beta <- object$beta()
  beta[object$D == 0.0] <- NA
  
  # TODO: replicate this error handle
  # if (tmp$ier!=0) stop("Error in REGCF: can't happen")
  
  beta
  
}


#' wrapper for `BoundedQr` method `vcov` 
#'
#' @param object BoundedQr object
#' @param ... ignored
#' @export
#' @keywords internal
vcov.Rcpp_BoundedQr <- function(object, ...) {
  
  np       <- object$num_params
  vcov_vec <- object$vcov(np)
  k        <- length(vcov_vec)
  V        <- matrix(nrow = np, ncol = np)
  pos      <- 1
  
  for(col in 1:np) {
    V[col, 1:col] <- vcov_vec[pos:(pos + col - 1)]
    pos <- pos + col
  }
  
  V[upper.tri(V)] <- t(V)[upper.tri(V)]
  
  V
}
