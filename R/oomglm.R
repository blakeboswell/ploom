#' #' @include oom_shared.R


#' @keywords internal
init_oomglm <- function(formula,
                        family,
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
  
  iwls <- list(
    beta = NULL,
    rss  = 0.0,
    deviance   = 0.0
  )
  
  obj <- list(
    call       = sys.call(-1),
    qr         = NULL,
    assign     = NULL,
    terms      = terms(formula),
    n          = 0,
    p          = NULL,
    names      = NULL,
    weights    = weights,
    df.resid   = NULL,
    sandwich   = xy,
    family     = family,
    iwls       = iwls,
    converged  = FALSE,
    iterations = 0L
  )
  
  class(obj) <- c('oomglm', 'oomlm')
  obj
  
}


#' @keywords internal
glm_adjust <- function(obj, chunk) {

  mm     <- chunk$data
  y      <- chunk$response
  w      <- chunk$weights
  offset <- chunk$offset

  fam    <- obj$family

  beta   <- obj$iwls$beta
  rss    <- obj$iwls$rss
  dev    <- obj$iwls$deviance

  if(is.null(beta)) {
    eta <- rep(0.0, nrow(mm)) + offset
  } else {
    eta <- mm %*% beta + offset
  }

  g       <- fam$linkinv(eta)
  gprime  <- fam$mu.eta(eta)
  z       <- eta + (y - g) / gprime
  fam_var <- fam$variance(g)
  w       <- w * gprime ^ 2 / fam_var

  if(!is.null(beta)) {
    rss <-
      rss + sum((y - g) ^ 2 / (w * fam_var)) * (sum(w) / length(w))
    dev <-
      dev + sum(fam$dev.resids(y, g, w))
  }

  list(
    z = z,
    w = w,
    g = g,
    deviance = dev,
    rss      = rss
  )

}


update_oomglm <- function(obj, data) {
  UseMethod("update_oomglm", data)
}
setGeneric("update_oomglm", signature=c("obj", "data"))


#' @export
update_oomglm.data.frame <- function(obj, data) {

  chunk <- unpack_oomchunk(obj, data)
  
  if(is.null(obj$assign)) {
    obj$assign <- chunk$assign
  }
  
  if(is.null(obj$qr)) {
    qr <- new_bounded_qr(chunk$p)
  } else {
    qr <- obj$qr
  }
  
  trans  <- glm_adjust(obj, chunk)
  
  obj$qr <- update(qr,
                   chunk$data,
                   trans$z - chunk$offset,
                   trans$w)
  
  if(!is.null(obj$sandwich)) {
    obj$sandwich$xy <-
      update_sandwich(obj$sandwich$xy,
                      chunk$data,
                      chunk$n,
                      chunk$p,
                      trans$z,
                      trans$w)
  }
  
  obj$n        <- obj$n + chunk$n
  obj$p        <- chunk$p
  obj$names    <- colnames(chunk$data)
  obj$df.resid <- obj$n - chunk$p
  
  obj$iwls$rss      <- trans$rss
  obj$iwls$deviance <- trans$deviance
  
  obj
  
}


#' @export
update_oomglm.function <- function(obj, data) {
  
  while(!is.null(chunk <- data())){
    obj <- update_oomglm(obj, chunk)
  }
  
  obj
  
}


#' @keywords internal
reset_oomglm <- function(obj) {
  
  obj$qr   <- NULL
  obj$n    <- 0L
  obj$iwls$rss      <- 0.0
  obj$iwls$deviance <- 0.0

  if(!is.null(obj$sandwich)) {
    obj$sandwich <- list(xy = NULL)
  }
 
}


#' @export
reweight_oomglm <- function(obj, data, num_iterations = 1L) {
  
  if(obj$converged) {
    return(obj)
  }
  
  for(i in 1:num_iterations) {
    
    beta_old <- obj$iwls$beta 
    
    obj <- reset_oomglm(obj)
    obj <- update_oomglm(obj, data)
    obj$iterations <- obj$iterations + 1L
    
    if(!is.null(beta_old)) {
      delta <- (beta_old - obj$iwls$beta) / sqrt(diag(vcov(obj)))
      obj$converged <- TRUE
      break
    }

  }
  
  obj
  
}


#' @export
oomglm <- function(formula,
                   data     = NULL,
                   family   = gaussian(),
                   weights  = NULL,
                   sandwich = FALSE) {

  obj <- init_oomglm(formula, family, weights, sandwich)

  if(!is.null(data)) {
    obj <- update_oomglm(obj, data)
  }

  obj

}
