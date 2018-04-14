#' @include oom_shared.R


#' @keywords internal
init_oomglm <- function(formula,
                        family,
                        weights,
                        etastart,
                        mustart,
                        sandwich) {
  
  if(is.character(family)) {
    family <- get(family, mode = "function", envir = parent.frame())
  }
  if(is.function(family)) {
    family <- family()
  }
  if(is.null(family$family)) {
    print(family)
    stop("'family' not recognized")
  }
  
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
    call          = sys.call(-1),
    qr            = NULL,
    assign        = NULL,
    terms         = terms(formula),
    n             = 0,
    names         = NULL,
    df.residual   = NULL,
    df.null       = NULL,
    sandwich      = xy,
    family        = family,
    iwls          = iwls,
    converged     = FALSE,
    iter          = 0L,
    weights       = weights,
    pweights      = 0,
    zero_weights  = 0
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
  
  
  zero_wts  <- trans$w == 0
  intercept <- attr(obj$terms, "intercept") > 0L
  
  obj$n            <- obj$n + chunk$n - sum(zero_wts)
  obj$names        <- colnames(chunk$data)
  obj$df.residual  <- obj$n - chunk$p
  obj$df.null      <- obj$n - as.integer(intercept)
  obj$pweights     <- obj$pweights + sum(log(trans$w[!zero_wts]))
  obj$zero_weights <- obj$zero_weights + sum(zero_wts)
  
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
reset_oomglm <- function(obj, beta_old) {
  
  obj$iwls$rss      <- 0.0
  obj$iwls$deviance <- 0.0
  obj$qr   <- NULL
  obj$n    <- 0L

  if(!is.null(obj$sandwich)) {
    obj$sandwich <- list(xy = NULL)
  }
  
  obj
 
}


#' @export
reweight_oomglm <- function(obj, data, num_iterations = 1L, tolerance=1e-7) {
  
  if(obj$converged) {
    return(obj)
  }
  
  for(i in 1:num_iterations) {
    
    beta_old <- coef(obj)
    obj      <- reset_oomglm(obj)
    
    obj            <- update_oomglm(obj, data)
    obj$iwls$beta  <- coef(obj)
    obj$iter <- obj$iter + 1L
    
    if(!is.null(beta_old)) {
      delta <- (beta_old - obj$iwls$beta) / sqrt(diag(vcov(obj)))
      if (max(abs(delta)) < tolerance){
        obj$converged <- TRUE
        break
      }
    }

  }
  
  obj
  
}


#' @export
oomglm <- function(formula,
                   data     = NULL,
                   family   = gaussian(),
                   weights  = NULL,
                   etastart = NULL,
                   mustart  = NULL,
                   sandwich = FALSE) {

  obj <- init_oomglm(formula,
                     family,
                     weights,
                     etastart,
                     mustart,
                     sandwich)

  if(!is.null(data)) {
    obj <- update_oomglm(obj, data)
  }

  obj

}


print.oomglm <- function() {
  
  cat("\nOut-of-memory Generalized Linear Model:\n",
      paste(deparse(obj$call), sep = "\n", collapse = "\n"),
      "\n\n",
      sep = "")
  
  beta <- coef(obj)
  
  if(length(beta)) {
    cat("Coefficients:\n")
    print.default(
      format(beta, digits = digits),
      print.gap = 2L,
      quote     = FALSE)
  } else {
    cat("No coefficients\n")
  }
  
  cat("\n")
  cat("Observations included: ", obj$n, "\n")
  
  invisible(obj)
  
}