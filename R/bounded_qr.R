
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


