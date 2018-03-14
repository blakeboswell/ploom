#' @include oommodel.R

#' @keywords internal
init_oomlm <- init_model(model_class = c('oomlm'))


#' @keywords internal
lm_transform <- function(obj, chunk) {
  list(
    z = chunk$response,
    w = chunk$weights
  )
}


#' @export
update_oomlm <- update_oommodel(response_transform = lm_transform)


#' Updating Linear Regression model
#' 
#' @description
#' 
#' 
#' @param formula A model formula: a symbolic description of the
#'   model to be fitted.
#' @param data an optional data frame, list or environment
#'   (or object coercible by as.data.frame to a data frame)
#' @param weights A one-sided, single term formula specifying weights
#' @param sandwich TRUE to compute the Huber/White sandwich covariance matrix
#'   (uses `p^4` memory rather than `p^2`)
#' @details The model formula must not contain any data-dependent terms, as
#'   these will not be consistent when updated. Factors are permitted, but 
#'   the levels of the factor must be the same across all data chunks. 
#'   Empty factor levels are accepted.
#' @return `oomlm` model object that can be updated with more data
#'   via [update_oomlm][ploom::update_oomlm]
#' @examples
#' # The function `oomlm` is similar to base `lm` for fitting in-memory data.
#'
#' w <- oomlm(mpg ~ cyl + disp, data = mtcars)
#'
#' # Models are initalized with a call to `oomlm` and updated with
#' # `update_oomlm`. The recommended pattern is to initialize models without
#' # referencing the data, then call `update_oomlm` on each data chunk in the 
#' # exact same way.
#' 
#' # proxy for big data feed (`purrr::pmap`)
#' chunks  <- purrr::pmap(mtcars, list)
#' 
#' # initialize the model
#' x <- oomlm(mpg ~ cyl + disp)
#' 
#' # iteratively update model with data chunks
#' for(chunk in chunks) {
#'   update_oomlm(x, chunk)
#' }
#'
#' # Separating model initialization and processing of the first data chunk
#' # enables functional patterns like `reduce` to take the place of loops.
#' # The below example is equivalent to the above `for` loop.
#' 
#' # avoid loops altogether with `purrr::reduce`
#' y <- purrr::reduce(chunks, update_oomlm, .init = oomlm(mpg ~ cyl + disp))
#' 
#' # For maximum flexibility, `ploom` also supports providing data on
#' # initialization similar to [`biglm`](https://github.com/cran/biglm).
#'
#' # initial fit
#' z  <- oomlm(mpg ~ cyl + disp, chunks[[1]])
#' 
#' # iteratively update model with additional data chunks
#' for(chunk in tail(chunks, -1)) {
#'   z <- update_oomlm(x, data = chunk)
#' }
#'
#' @export
oomlm <- function(formula,
                  data     = NULL,
                  weights  = NULL,
                  sandwich = FALSE) {
  
  family <- NULL
  
  obj <- init_oomlm(formula, family, weights, sandwich)
  
  if(!is.null(data)) {
    obj <- update_oomlm(obj, data)
  }
  
  obj
  
}


