

#' @keywords internal
etafun <- function(x, beta) {
  if (is.null(beta)) {
    rep(0, nrow(x))
  } else {
    x %*% beta
  }
}


#' @keywords internal
glm_func <- function(chunk, obj) {
  
  mm     <- chunk$data
  y      <- chunk$response
  w      <- chunk$weights
  offset <- chunk$offset
  
  family <- obj$family
  beta   <- obj$fitstats$beta
  
  eta <- etafun(mm, beta) + offset
  mu  <- family$linkinv(eta)
  dmu <- family$mu.eta(eta)
  z   <- eta + (y - mu) / dmu
  w   <- w * dmu * dmu / family$variance(mu)
  
  list(
    z = z,
    w = w
  )
  
}



#' @keywords internal
chunk_unpack <- function(chunk, obj) {
  
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
sandwich_update <- function(chunk, qr) {
  
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
model_update <- function(data, obj, func) {
  
  chunk <- chunk_unpack(data, obj)
  
  if(is.null(obj$assign)) {
    obj$assign <- chunk$assign
  }
  
  if(is.null(obj$qr)) {
    qr <- new_bounded_qr(chunk$p)
  } else {
    qr <- obj$qr
  }
  
  link   <- func(chunk, obj)
  obj$qr <- update(qr, chunk$data, link$z - chunk$offset, link$w)
 
  if(!is.null(obj$sandwich)) {
    obj$sandwich$xy <- sandwich_update(chunk, obj$sandwich$xy)
  }
  
  obj$n           <- obj$n + chunk$n
  obj$names       <- colnames(chunk$data)
  obj$df.resid    <- obj$n - chunk$p

  obj
  
}
