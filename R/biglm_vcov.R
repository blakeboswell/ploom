
#' Update HC sandwich estimator
#' 
#' @param qr BoundedQr object
#' @param mm 
#' @param n number of observations
#' @param p number of covariates
#' @param y response
#' @param offset response offset
#' @param w weights
#' 
#' @keywords internal
update_sandwich <- function(qr, mm, n, p, y, offset, w) {
  
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


#' Hubert/White sandwich vcov implementation for updating linear
#' model (Miller AS274 QR factorization) as implemented 
#' in package `biglm`
#' 
#' @param np number of parameters in model
#' @param D diagonals of cross products matrix for sandwich estimator
#' @param rbar the off diagonal portion of the R matrix for sandwich estimator
#' @param R cov matrix for model
#' @param betas model coefficients
#' @param ok boolean indicator of linear independence among coefficients
#' 
#' @keywords internal
sandwich_rcov_biglm <- function(np, D, rbar, R, betas, ok) {
  
  rxy  <- diag(np * (np + 1))
  rxy[row(rxy) > col(rxy)] <- rbar
  rxy <- t(rxy)
  rxy <- sqrt(D) * rxy
  M   <- t(rxy) %*% rxy
  
  betas[!ok] <- 0
  bbeta      <- kronecker(diag(np), betas)
  
  ##FIXME: singularities in beta
  Vcenter <- (
    M[1:np, 1:np, drop = FALSE]
    + t(bbeta)
    %*% M[-(1:np), -(1:np), drop = FALSE]
    %*% bbeta
    - t(bbeta)
    %*% M[-(1:np), 1:np, drop = FALSE]
    - M[1:np, -(1:np), drop = FALSE]
    %*% bbeta
  )
  
  V <- matrix(NA, np, np)
  
  V[ok, ok] <- (
    R[ok, ok, drop = FALSE]
    %*% Vcenter[ok, ok, drop = FALSE]
    %*% R[ok, ok, drop = FALSE]
  )
  
  V
  
}
