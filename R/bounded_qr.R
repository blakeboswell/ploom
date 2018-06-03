#' initialize new `BoundedQr` object
#'
#' @param np int number of independent parameters in model
#'   including the intercept when model has intercept
#' @keywords internal
new_bounded_qr <- function(np) {
  
  if(np < 1) {
    stop("Number of parameters must be greater than 0")
  }
  
  methods::new(Class = BoundedQr, np)
}


#' wrapper for `BoundedQr` method `update`
#'
#' @param qr BoundedQr object to update
#' @param X numeric matrix of covariate observations
#' @param y numeric vector of response
#' @param weights numeric vector of observation weights
#' @keywords internal
update.Rcpp_BoundedQr <- function(qr, X, y, weights) {
  
  if(ncol(X) != qr$num_params) {
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
  
  qr$update(X, y, weights)
  qr
}


#' wrapper for `BoundedQr` method `betas`
#' 
#' @param qr BoundedQr object
#' @param nvar int number of coefficients to return
#' @param ... ignored
#' @keywords internal
coef.Rcpp_BoundedQr <- function(qr, nvar = NULL, ...){
  
  if (is.null(nvar)) {
    nvar <- qr$num_params
  }
    
  if (nvar < 1 | nvar > qr$num_params) {
    stop("Invalid value of `nvar`")
  }
  
  beta              <- qr$beta()
  beta[qr$D == 0.0] <- NA
  
  # TODO: replicate this error handle
  # if (tmp$ier!=0) stop("Error in REGCF: can't happen")
  
  beta
  
}


#' wrapper for `BoundedQr` method `vcov` 
#'
#' @param qr BoundedQr object
#' @keywords internal
vcov.Rcpp_BoundedQr <- function(qr) {
  
  vcov_vec <- qr$vcov(qr$num_params)
  k        <- length(vcov_vec)
  np       <- qr$num_params
  V        <- matrix(nrow = np, ncol = np)
  pos      <- 1
  
  for(col in 1:np) {
    V[col, 1:col] <- vcov_vec[pos:(pos + col - 1)]
    pos <- pos + col
  }
  
  V[upper.tri(V)] <- t(V)[upper.tri(V)]
  
  V
}
