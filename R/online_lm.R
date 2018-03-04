

#' Linear model that uses only `p^2` memory for `p` variables.
#' It can be updated with more data using `update` allowing for linear
#' regression on data sets larger than memory.
#' 
#' @param data
#' @param formula
#' @param weights
#' @param sandwich
#' @export
online_lm <- function(data,
                      formula,
                      weights  = NULL,
                      sandwich = FALSE) {

  model_terms <- terms(formula)
  model_data  <- model.frame(model_terms, data)
  
  if(is.null(offset <- model.offset(model_data))) {
    offset <- 0
  }
  
  model_response <- model.response(model_data) - offset
  model_data     <- model.matrix(model_terms, model_data)
  
  p <- ncol(model_data)
  n <- nrow(model_data) 

  if(is.null(weights)) {
    weights <- rep(1.0, n)
  } else {
    if(!inherits(weights, "formula")) {
      stop("`weights` must be a formula")
    }
    weights <- model.frame(weights, data)[[1]]
  }
  
  model_qr <- update(new_bounded_qr(p),
                     model_data, model_response, weights)

  rval <- list(
    call     = sys.call(),
    qr       = model_qr,
    assign   = attr(model_data, "assign"),
    terms    = model_terms,
    n        = n,
    names    = colnames(model_data),
    weights  = weights,
    df.resid = n - length(model_qr$D)
  )

  if (sandwich) {
    xx        <- matrix(nrow = n, ncol = p * (p + 1))
    xx[, 1:p] <- model_data * model_response
    for (i in 1:p) {
      xx[, p * i + (1:p)] <- model_data * model_data[, i]
    }
    
    sandwich_qr <- update(new_bounded_qr(p * (p + 1)),
                          xx, rep(0, n), weights ^ 2)
    rval$sandwich <- list(xy = sandwich_qr)
  }

  class(rval) <- "online_lm"
  rval

}


#' @export
coef.online_lm <- function(obj, ...) {
  betas        <- coef(obj$qr)
  names(betas) <- obj$names
  betas
}


#' @export
vcov.online_lm <- function(obj, ...) {
  
  ## cpp implementation / HC not supported ---------------------------
  # V <- vcov(obj$qr)
  # dimnames(V) <- list(obj$names, obj$names)
  # 
  # V
  
  # biglm implementation --------------------------------------------
  
  obj$qr$singcheck()
  
  ok    <- obj$qr$D != 0.0
  nobs  <- obj$qr$nobs
  np    <- obj$qr$np
  sserr <- obj$qr$sserr
  
  R  <- rvcov_biglm(
    obj$qr$np,
    obj$qr$D,
    obj$qr$rbar,
    ok
  )
  
  dimnames(R) <- list(obj$names, obj$names)
  
  if(!is.null(obj$sandwich)) {
    
    betas <- coef(obj$qr)
    
    V <- sandwich_rcov_biglm(
      R,
      np,
      obj$sandwich$xy$D,
      obj$sandwich$xy$rbar,
      betas,
      ok
    )
    
    dimnames(V) <- list(obj$names, obj$names)
    attr(V, "model-based") <- R * sserr / (nobs - np + sum(!ok))
    
  } else {
    V <- R * sserr / (nobs - np + sum(!ok))
  }
  
  V

}


#' @export
deviance.online_lm <- function(obj, ...) {
  obj$qr$sserr
}


#' @export
print.online_lm <- function(obj, ...) {
  cat("Online linear regression model: ")
  print(obj$call)
  cat("Observations included = ", obj$n, "\n")
  invisible(obj)
}


#' @export
summary.online_lm <- function(obj, ...) {
  beta <- coef(obj)
  se   <- sqrt(diag(vcov(obj)))
  mat  <- cbind(
    `Coef` = beta,
    `(95%` = beta - 2 * se,
    `CI)`  = beta + 2 * se,
    `SE`   = se,
    `p`    = 2 * pnorm(abs(beta / se), lower.tail = FALSE)
  )
  rownames(mat) <- obj$names
  rval <- list(obj = obj, mat = mat)
  if (attr(obj$terms, "intercept")) {
    rval$nullrss <-
      obj$qr$sserr + sum(obj$qr$D[-1] * obj$qr$thetab[-1] ^ 2)
  } else {
    rval$nullrss <-
      obj$qr$sserr + sum(c(obj$qr$D) * c(obj$qr$thetab) ^ 2)
  }

  rval$rsq    <- 1 - deviance(obj) / rval$nullrss
  class(rval) <- "summary.online_lm"
  rval

}


#' @export
print.summary.online_lm <- function(obj, digits = getOption("digits") - 3, ...) {
  print(obj$obj)
  print(round(obj$mat, digits))
  if (!is.null(obj$obj$sandwich)) {
    cat("Sandwich (model-robust) standard errors\n")
  }
  invisible(obj)
}



# update.online_lm <- function(object, moredata, ...) {
#   
#   mf <- model.frame(object$terms, moredata)
#   mm <- model.matrix(object$terms, mf)
#   
#   if (is.null(object$weights)) {
#     w <- NULL
#   } else {
#     w <- model.frame(object$weights, moredata)[[1]]
#   }
#   
#   if (!identical(object$assign, attr(mm, "assign"))) {
#     stop("model matrices incompatible")
#   }
#   
#   if (is.null(off <- model.offset(mf))) {
#     off <- 0
#   }
#   
#   update.online_qr(object$qr, mm, model.response(mf) - off, w)
#   object$n <- object$n + nrow(mm)
#   
#   if (!is.null(object$sandwich)) {
#     
#     p  <- ncol(mm)
#     n  <- nrow(mm)
#     xx <- matrix(nrow = n, ncol = p * (p + 1))
#     xx[, 1:p] <- mm * (model.response(mf) - off)
#     for (i in 1:p) {
#       xx[, p * i + (1:p)] <- mm * mm[, i]
#     }
#     xyqr <- update(object$sandwich$xy, xx, rep(0, n), w * w)
#     object$sandwich <- list(xy = xyqr)
#     
#   }
#   object
# }



