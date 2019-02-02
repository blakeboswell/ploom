
#' @importFrom Rcpp evalCpp
#' @useDynLib ploom
#' @exportPattern "^[[:alpha:]]+"
#' @importFrom stats
#'  AIC
#'  as.formula
#'  BIC 
#'  coef
#'  confint
#'  confint.lm 
#'  contrasts<- 
#'  deviance
#'  family
#'  formula
#'  gaussian
#'  logLik
#'  makepredictcall
#'  .MFclass
#'  na.fail
#'  pf
#'  printCoefmat
#'  pt
#'  qt
#'  symnum
#'  terms 
#' @importFrom utils head read.table tail getFromNamespace
NULL


#' @importFrom generics tidy
#' @export
generics::tidy

#' @importFrom generics fit
#' @export
generics::fit

#' @importFrom generics glance
#' @export
generics::glance

#' @importFrom generics augment
#' @export
generics::augment