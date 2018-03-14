
#' @keywords internal
unpack_oomchunk <- function(obj, chunk) {
  
  model_terms  <- obj$terms
  chunk_data   <- model.frame(model_terms, chunk)
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
    chunk_weights <- model.frame(obj$weights, chunk_data)[[1]]
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
update_sandwich <- function(qr, chunk) {
  
  mm <- chunk$data
  y  <- chunk$response
  w  <- chunk$weights
  n  <- chunk$n
  p  <- chunk$p
  
  xx        <- matrix(nrow = n, ncol = p * (p + 1))
  xx[, 1:p] <- mm * y
  
  for (i in 1:p) {
    xx[, p * i + (1:p)] <- mm * mm[, i]
  }
  
  if(is.null(qr)) {
    qr <- new_bounded_qr(p * (p + 1)) 
  }
  
  update(qr, xx, rep(0, n), w * w)
  
}


#' @keywords internal
update_oommodel <- function(transform) {
  
  function(obj, chunk) {
    chunk <- unpack_oomchunk(obj, chunk)
    
    if(is.null(obj$assign)) {
      obj$assign <- chunk$assign
    }
    
    if(is.null(obj$qr)) {
      qr <- new_bounded_qr(chunk$p)
    } else {
      qr <- obj$qr
    }
    
    trans  <- transform(obj, chunk)
    obj$qr <- update(qr, chunk$data, trans$z - chunk$offset, trans$w)
    
    if(!is.null(obj$sandwich)) {
      obj$sandwich$xy <- update_sandwich(obj$sandwich$xy, chunk)
    }
    
    obj$n           <- obj$n + chunk$n
    obj$names       <- colnames(chunk$data)
    obj$df.resid    <- obj$n - chunk$p
    
    obj
  }

}



#' @keywords  internal
model_init <- function(model_class) {
  
  function(formula,
           family   = NULL,
           weights  = NULL,
           sandwich = FALSE) {
    
    if(!is.null(weights) && !inherits(weights, "formula")) {
      stop("`weights` must be a formula")
    }
    
    if(sandwich) {
      xy <- list(xy = NULL)
    } else {
      xy <- NULL
    }
    
    obj <- list(
      call     = sys.call(-1),
      qr       = NULL,
      family   = family,
      assign   = NULL,
      terms    = terms(formula),
      n        = 0,
      p        = NULL,
      names    = NULL,
      weights  = weights,
      df.resid = NULL,
      sandwich = xy
    )
    
    class(obj) <- model_class
    obj
    
  }

}
