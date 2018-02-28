
#' #' @internal
#' bounded_qr <- R6::R6Class(
#'   "bounded_qr",
#'   cloneable = FALSE,
#'   portable  = FALSE,
#'   public = list(
#'     qr = NULL,
#'     initialize = function(np) {
#'       self$qr <- new(Class = BoundedQr, np)
#'     }
#'   )
#' )



#' @internal
new_bounded_qr <- function(np) {
  qr <- new(Class = BoundedQr, np)
  e        <- environment()
  class(e) <- "bounded_qr"
  e
}


#' @export 
#'
#' @param qr bounded_qr object to update
#' @param X matrix of covariate observations
#' @param y response
#' @param weights weights of each observation
update.bounded_qr <- function(obj, X, y, weights) {
  # error check
  obj$qr$update(X, y, weights)
  obj
}


