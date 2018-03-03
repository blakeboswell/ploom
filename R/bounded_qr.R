
#' @internal
new_bounded_qr <- function(p) {
  new(Class = BoundedQr, p)
}


#' @export 
#'
#' @param qr Rcpp_BoundedQr object to update
#' @param X matrix of covariate observations
#' @param y response
#' @param weights weights of each observation
update.Rcpp_BoundedQr <- function(qr, X, y, weights) {
  qr$update(X, y, weights)
  qr
}


coef.Rcpp_BoundedQr <- function(qr, nvar = NULL, ...){
  
  p <- length(qr$D)
  
  if (is.null(nvar)) {
    nvar <- p
  }
    
  if (nvar < 1 | nvar > p) {
    stop("Invalid value of `nvar`")
  }
  
  if (!qr$tolset) {
    qr$check_singularity()
  }
  
  # TODO: replicate this error handle
  # if (tmp$ier!=0) stop("Error in REGCF: can't happen")
  
  beta              <- qr$betas()
  beta[qr$D == 0.0] <- NA
  
  beta
  
}


vcov.Rcpp_BoundedQr <- function(qr) {
  
  qr$vcov(length(qr$D))
  
}



