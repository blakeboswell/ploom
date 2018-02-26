
#' @internal
bounded_qr <- R6::R6Class(
  "bounded_qr",
  cloneable = FALSE,
  portable  = FALSE,
  public = list(
    qr = NULL,
    initialize = function(np) {
      self$qr <- new(Class = BoundedQR, np)
    }
  )
)


#' @export 
#'
#' @param qr bounded_qr object to update
#' @param X matrix of covariate observations
#' @param y response
#' @param weights weights of each observation
update.bounded_qr <- function(qr, X, y, weights) {
  # error check
  qr$update(X, y, weights)
  qr
}


