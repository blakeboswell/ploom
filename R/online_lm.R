

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
update.online_lm <- function(data, obj, ...) {

  mf <- model.frame(obj$terms, data)
  mm <- model.matrix(obj$terms, mf)

  if (is.null(obj$weights)) {
    w <- NULL
  } else {
    w <- model.frame(obj$weights, moredata)[[1]]
  }

  if (!identical(obj$assign, attr(mm, "assign"))) {
    stop("model matrices incompatible")
  }

  if (is.null(off <- model.offset(mf))) {
    off <- 0
  }

  update(obj$qr, mm, model.response(mf) - off, w)
  obj$n <- obj$n + nrow(mm)

  if (!is.null(obj$sandwich)) {

    p  <- ncol(mm)
    n  <- nrow(mm)
    xx <- matrix(nrow = n, ncol = p * (p + 1))
    xx[, 1:p] <- mm * (model.response(mf) - off)
    for (i in 1:p) {
      xx[, p * i + (1:p)] <- mm * mm[, i]
    }
    xyqr <- update(obj$sandwich$xy, xx, rep(0, n), w * w)
    obj$sandwich <- list(xy = xyqr)

  }
  
  obj
  
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
  
  nobs  <- obj$qr$nobs
  np    <- obj$qr$np
  sserr <- obj$qr$sserr
  ok    <- obj$qr$D != 0
  
  R  <- rvcov_biglm(
    np,
    obj$qr$D,
    obj$qr$rbar,
    ok
  )
  
  dimnames(R) <- list(obj$names, obj$names)
  
  if(!is.null(obj$sandwich)) {
    
    betas <- coef(obj$qr)
    
    V <- sandwich_rcov_biglm(
      np,
      obj$sandwich$xy$D,
      obj$sandwich$xy$rbar,
      R,
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
print.online_lm <- function(obj,
                            digits = max(3L, getOption("digits") - 3L),
                            ...) {
  
  cat("\nCall:\n",
      paste(deparse(obj$call), sep = "\n", collapse = "\n"),
      "\n\n",
      sep = "")
  
  betas <- coef(obj)
  
  if(length(betas)) {
    cat("Coefficients:\n")
    print.default(
      format(betas, digits = digits),
      print.gap = 2L,
      quote     = FALSE)
  } else {
    cat("No coefficients\n")
  }
  
  cat("\n")
  cat("Observations included: ", obj$n, "\n")

  invisible(obj)
  
}


summary.online_lm <- function(obj, ...) {
  # https://github.com/wch/r-source/blob/trunk/src/library/stats/R/lm.R
  
  beta <- coef(obj)
  se   <- sqrt(diag(vcov(obj)))
  tval <- beta/se
  rdf  <- obj$qr$nobs - obj$qr$np
  
  mat <- cbind(
    "Estimate"   = beta,
    "Std. Error" = se,
    "t value"    = tval,
    "Pr(>|t|)"   = 2 * pt(abs(tval), rdf, lower.tail = FALSE)
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
  
  rval$r.squared    <- 1 - deviance(obj) / rval$nullrss
  class(rval) <- "summary.online_lm"
  rval
  
  
}


#' #' @export
#' summary.online_lm <- function(obj, ...) {
#'   beta <- coef(obj)
#'   se   <- sqrt(diag(vcov(obj)))
#'   mat  <- cbind(
#'     `Coef` = beta,
#'     `(95%` = beta - 2 * se,
#'     `CI)`  = beta + 2 * se,
#'     `SE`   = se,
#'     `p`    = 2 * pnorm(abs(beta / se), lower.tail = FALSE)
#'   )
#'   rownames(mat) <- obj$names
#'   rval <- list(obj = obj, mat = mat)
#'   if (attr(obj$terms, "intercept")) {
#'     rval$nullrss <-
#'       obj$qr$sserr + sum(obj$qr$D[-1] * obj$qr$thetab[-1] ^ 2)
#'   } else {
#'     rval$nullrss <-
#'       obj$qr$sserr + sum(c(obj$qr$D) * c(obj$qr$thetab) ^ 2)
#'   }
#' 
#'   rval$r.squared    <- 1 - deviance(obj) / rval$nullrss
#'   class(rval) <- "summary.online_lm"
#'   rval
#' 
#' }


#' @export
print.summary.online_lm <- function(obj,
                                    digits = max(3L, getOption("digits") - 3L),
                                    ...) {
  print(obj$obj)
  print(round(obj$mat, digits))
  if (!is.null(obj$obj$sandwich)) {
    cat("Sandwich (model-robust) standard errors\n")
  }
  invisible(obj)
}




