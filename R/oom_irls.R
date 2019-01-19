

#' Reweighting functions that comprise an IRLS iteration
#' 
#' @details
#' The IRLS process comprises iterative calls to [init_weight()], [weight()]
#' and [end_weight()] which can be managed via a single call to [iter_weight()].
#' These comprising functions are exposed for lower level control of the 
#' fitting process.
#'
#' @md
#' @param object [oomglm()] model.
#' @param data an optional [oomdata_tbl()], [oomdata_dbi()], [oomdata_con()],
#'   [tibble()], [data.frame()], or [list()] of observations to fit
#' @param tolerance Tolerance used to determine convergence. Represents
#'  change in coefficient as a multiple of standard error. 
#' @return [oomglm()] model
#' 
#' @seealso  [iter_weight()], [oomglm()]
#' @export
#' @examples \donttest{
#' # initiate `oomglm()` model and `oomdata_tbl()`
#' x <- oomglm(mpg ~ cyl + disp)
#' chunks <- oomdata_tbl(mtcars, chunk_size = 10)
#' 
#' # manually perform one round of IRLS and check results
#' x <- init_weight(x)
#' x <- weight(x, data = chunks)
#' x <- end_weight(x)
#' 
#' print(x$converged)
#' print(coef(x))
#' 
#' # manually perform an additional round of IRLS
#' x <- init_weight(x)
#' x <- weight(x, data = chunks)
#' x <- end_weight(x)
#' 
#' print(x$converged)
#' print(coef(x))
#' 
#' # the IRLS process as implemented by `iter_weight()`
#' # is similar to the following
#' x <- oomglm(mpg ~ cyl + disp)
#' chunks <- oomdata_tbl(mtcars, chunk_size = 10)
#' 
#' while(!x$converged) {
#'   
#'   x <- init_weight(x)
#'   x <- weight(x, data = chunks)
#'   x <- end_weight(x) 
#'   
#' }
#' 
#' tidy(x)
#' 
#' }
#' @name init_weight
NULL
