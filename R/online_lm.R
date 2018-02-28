
#' extract weights from online_lm inputs
#' 
#' @param weights a formula object
#' @param data a data.frame, list, or environment (or object coercible
#'      by as.data.frame to a data.frame), containing the variables in formula.
#'      Neither a matrix nor an array will be accepted.
#' @param n number of rows to be weighted
#' 
#' @internal
prepare_weights <- function(weights, data, n) {

  if(is.null(weights)) {
    return(rep(1.0, n))
  }

  if(!inherits(weights, "formula")) {
    stop("`weights` must be a formula")
  }

  model.frame(weights, data)[[1]]

}


#' prepare model inputs
#'
#' @param formula
#' @param data
#' @param weight
#' 
#' @internal
prepare_model <- function(data, formula, weights) {

  model_terms <- terms(formula)
  model_data  <- model.frame(model_terms, data)

  if(is.null(offset <- model.offset(model_data))) {
    offset <- 0
  }

  model_response <- model.response(model_data) - offset
  model_weights  <- prepare_weights(weights, data, length(model_response))
  model_data     <- model.matrix(model_terms, model_data)

  list(
    model_terms    = model_terms,
    model_matrix   = model_data,
    model_response = model_response,
    weights        = model_weights
  )

}


#' @export
#'
#' @param data
#' @param formula
#' @param weights
#' @param sandwich
online_lm <- function(data,
                      formula,
                      weights  = NULL,
                      sandwich = FALSE) {

  data <- prepare_model(data, formula, weights)
  qr   <- new_bounded_qr(ncol(data$model_matrix))
  qr   <- update(qr,
                 data$model_matrix,
                 data$model_response,
                 data$weights)

  rval <- list(
    call    = sys.call(),
    qr      = qr,
    assign  = attr(data$model_data, "assign"),
    terms   = data$model_terms,
    n       = nrow(data$model_data),
    names   = colnames(data$model_data),
    weights = data$weights
  )

  if (sandwich) {

    # p    <- ncol(mm)
    # n    <- nrow(mm)
    # xyqr <- bounded_qr(p * (p + 1))
    # 
    # xx   <- matrix(nrow = n, ncol = p * (p + 1))
    # xx[, 1:p] <- mm * data$model_response
    # for (i in 1:p) {
    #   xx[, p * i + (1:p)] <- mm * mm[, i]
    # }
    # 
    # xyqr <- update_qr(xyqr, xx, rep(0, n), data$weights ^ 2)
    # rval$sandwich <- list(xy = xyqr)

  }

  rval$df.resid <- rval$n - length(qr$D)
  class(rval) <- "online_lm"
  rval

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


# coef.online_lm <- function(object, ...) {
#   if (!object$qr$tol_set) {
#     object$qr$singchk()
#   }
#   rval <- coef(object$qr)
#   rval[object$qr$D == 0] <- NA
#   names(rval) <- object$names
#   rval
# }

# vcov.online_lm <- function(object, ...) {
#   
#   if (!object$qr$tol_set) {
#     object$qr$singchk()
#   }
#   
#   p         <- length(object$qr$D)
#   R         <- diag(p)
#   R[row(R) > col(R)] <- object$qr$rbar
#   R         <- t(R)
#   R         <- sqrt(object$qr$D) * R
#   ok        <- object$qr$D != 0
#   R[ok, ok] <- chol2inv(R[ok, ok, drop = FALSE])
#   R[!ok, ]  <- NA
#   R[, !ok]  <- NA
#   dimnames(R) <- list(object$names, object$names)
#   
#   if (!is.null(object$sandwich)) {
#     
#     object$sandwich$xy$singchk()
#     
#     rxy  <- diag(p * (p + 1))
#     rxy[row(rxy) > col(rxy)] <- object$sandwich$xy$rbar
#     rxy <- t(rxy)
#     rxy <- sqrt(object$sandwich$xy$D) * rxy
#     M   <- t(rxy) %*% rxy
#     
#     beta <- coef(object)
#     beta[!ok] <- 0
#     bbeta <- kronecker(diag(p), beta)
#     
#     ##FIXME: singularities in beta
#     Vcenter <-
#       M[1:p, 1:p, drop = FALSE] + t(bbeta) %*% M[-(1:p), -(1:p), drop = FALSE] %*%
#       bbeta -
#       t(bbeta) %*% M[-(1:p), 1:p, drop = FALSE] - M[1:p, -(1:p), drop =
#                                                       FALSE] %*% bbeta
#     
#     V <- matrix(NA, p, p)
#     V[ok, ok] <-
#       R[ok, ok, drop = FALSE] %*% Vcenter[ok, ok, drop = FALSE] %*% R[ok, ok, drop =
#                                                                         FALSE]
#     dimnames(V) <- list(object$names, object$names)
#     attr(V, "model-based") <- R * object$qr$sserr / (object$n - p + sum(!ok))
#     
#   } else {
#     V <- R * object$qr$sserr / (object$n - p + sum(!ok))
#   }
#   
#   V
# }




print.online_lm <- function(x, ...) {
  cat("Bounded memory regression model: ")
  print(x$call)
  cat("Observations included = ", x$n, "\n")
  invisible(x)
}


summary.online_lm <- function(object, ...) {
  beta <- coef(object)
  se   <- sqrt(diag(vcov(object)))
  mat  <- cbind(
    `Coef` = beta,
    `(95%` = beta - 2 * se,
    `CI)`  = beta + 2 * se,
    `SE`   = se,
    `p`    = 2 * pnorm(abs(beta / se), lower.tail = FALSE)
  )
  rownames(mat) <- object$names
  rval <- list(obj = object, mat = mat)
  if (attr(object$terms, "intercept")) {
    rval$nullrss <-
      object$qr$sserr + sum(object$qr$D[-1] * object$qr$thetab[-1] ^ 2)
  } else {
    rval$nullrss <-
      object$qr$sserr + sum(c(object$qr$D) * c(object$qr$thetab) ^ 2)
  }
  
  rval$rsq    <- 1 - deviance(object) / rval$nullrss
  class(rval) <- "summary.online_lm"
  rval
  
}


print.summary.online_lm <- function(x, digits = getOption("digits") - 3, ...) {
  print(x$obj)
  print(round(x$mat, digits))
  if (!is.null(x$obj$sandwich))
    cat("Sandwich (model-robust) standard errors\n")
  invisible(x)
}

