
#' @keywords internal
unpack_oomchunk <- function(obj, data) {
  
  model_terms  <- obj$terms
  chunk_data   <- model.frame(model_terms, data)
  chunk_assign <- attr(chunk_data, "assign")
  
  if(!is.null(obj$assign)) {
    if (!identical(obj$assign, chunk_assign)) {
      stop("model matrices incompatible")
    }
  }
  
  if(is.null(chunk_offset <- model.offset(chunk_data))) {
    chunk_offset <- 0.0
  }
  
  chunk_response <- model.response(chunk_data) - chunk_offset
  chunk_data     <- model.matrix(model_terms, chunk_data)
  
  p <- ncol(chunk_data)
  n <- nrow(chunk_data)
  
  if(is.null(obj$weights)) {
    chunk_weights <- rep(1.0, n)
  } else {
    chunk_weights <- model.frame(obj$weights, data)[[1]]
  }
  
  list(
    data     = chunk_data,
    n        = n,
    p        = p,
    response = chunk_response,
    weights  = chunk_weights,
    offset   = chunk_offset,
    assign   = chunk_assign
  )
  
}


#' @keywords internal
update_sandwich <- function(qr, mm, n, p, y, w) {
  
  xx        <- matrix(nrow = n, ncol = p * (p + 1))
  xx[, 1:p] <- mm * (drop(y) - offset)
  
  for (i in 1:p) {
    xx[, p * i + (1:p)] <- mm * mm[, i]
  }
  
  if(is.null(qr)) {
    qr <- new_bounded_qr(p * (p + 1)) 
  }
  
  update(qr, xx, rep(0.0, n), w * w)
  
}
