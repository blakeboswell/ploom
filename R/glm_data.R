
#' Intenal. For generating GLM data used in tests and vignettes.
#' 
#' Make a and B for y = a + BX + e.
#'
#' @param p number of columns in X.  will be forced to the nearest
#'   number divisible by two
#' @param betas_mu_lim mean of beta normal distributions
#'   range between `-betas_mu_lim` and `betas_mu_lim`
#' @param betas_sd_lim standard deviation of beta normal distributions
#'   range between 1 and `betas_sd_lim`
#'
#' @keywords internal
linear_params <- function(p,
                          betas_mu_lim  = 0,
                          betas_sd_lim  = 1) {
  
  p <- 2 * ceiling(p / 2)
  
  betas_mu  <- runif(p + 1, -betas_mu_lim, betas_mu_lim)
  betas_sd  <- runif(p + 1, 1, betas_sd_lim)
  
  X <- mapply(function(x, y) rnorm(1, x, y), x = betas_mu, y = betas_sd)
  
  col_names <- c(
    paste("real", 1:(p/2 + 1), sep = "_"),
    paste("int", 1:(p/2), sep = "_")
  )
  
  list(
    betas = matrix(X, ncol = 1),
    names = col_names
  )
  
}


#' Intenal. For generating GLM data used in tests and vignettes.
#' 
#' Transform param list into single row table
#' with alpha and B having names corresponding to data
#'
#' @param params coefficient values
#' @param col_names names of the coefficients
#'
#' @keywords internal
params_as_tibble <- function(params) {
  df <- tibble::as_tibble(
    matrix(params$betas, nrow = 1)
  )
  colnames(df) <- params$names
  df
} 


#' Intenal. For generating GLM data used in tests and vignettes.
#'
#' Create data with generalized linear relationship.
#'
#' @param betas coefficients
#' @param nrows number of observations to generate
#' 
#' @keywords internal
generate_data <- function(betas, nrows) {
  
  p <- (length(betas) - 1) / 2
  
  A  <- matrix(rep(1.0, nrows), ncol = 1)
  colnames(A) <- "intercept"
  
  X1 <- matrix(
    rnorm(nrows * p, mean = 0, sd = 1),
    nrow = nrows,
    ncol = p
  ) 
  colnames(X1) <- paste("real", 1:p, sep = "_")
  
  X2 <- mapply(
    function(num_cat, n) {
      sample(1:num_cat, n, replace = TRUE)
    },
    num_cat = 2:(p+1),
    n = nrows
  )
  colnames(X2) <- paste("int", 1:p, sep = "_")
  
  X       <- cbind(A, X1, X2)
  
  Y       <- X %*% betas
  Y_gauss <- rnorm(nrows, Y, 1)
  Y_bin   <- rbinom(nrows, 1, 1 / (1 + exp(-Y)))
  
  Y       <- X[, 1:(p + 1)] %*% betas[1:(p + 1)]
  Y_pois  <- rpois(nrows, exp(Y))
  Y_gamma <- rgamma(nrows, rate = 10 / exp(Y), shape = 10)
  
  Y_all <- cbind(Y, Y_gauss, Y_bin, Y_pois, Y_gamma)
  colnames(Y_all) <- c("Y", "Y_gauss", "Y_bin", "Y_pois", "Y_gamma")
  
  df <- tibble::as_tibble(cbind(Y_all, X))
  colnames(df) <- tolower(colnames(df))
  df[, (colnames(df) != "intercept")]
  
}