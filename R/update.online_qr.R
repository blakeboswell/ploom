
update.online_qr <-function(qr, X, y, w = NULL,
                            singcheck = FALSE,
                            ...) {
  
  if (ncol(X) != length(qr$D)) {
    stop("Wrong number of columns")
  }
  
  if (length(y) != nrow(X)) {
    stop("Wrong number of rows")
  }
  
  if (length(w) == 0) {
    w <- rep(1.0, length(y))
  }
    
  if (length(y) != length(w)) {
    stop("`weights` has wrong length")
  }
  
  update_miller(X, y, w, qr)

  if (singcheck) {
   qr$singchk()
  }

}


