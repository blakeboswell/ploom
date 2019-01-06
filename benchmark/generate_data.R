
library(dplyr)
library(purrr)
library(stringr)


#' make a and B for y = a + BX + e
#'
#' @param p number of columns in X.  will be forced to the nearest
#'   number divisible by two
#' @param alpha_lim alpha will be randomly picked in
#'   range between `-alpha_lim` and `alpha_lim`
#' @param betas_mu_lim mean of beta normal distributions
#'   range between `-betas_mu_lim` and `betas_mu_lim`
#' @param betas_sd_lim standard deviation of beta normal distributions
#'   range between 1 and `betas_sd_lim`
linear_params <- function(p, 
                          alpha_lim     = 10,
                          betas_mu_lim  = 10,
                          betas_sd_lim  = 4) {
  
  p     <- p + (2 - p %% 2)
  
  betas_mu  <- runif(p, -betas_mu_lim, betas_mu_lim)
  betas_sd  <- runif(p, 1, betas_sd_lim)
  
  X <- map2_dbl(betas_mu, betas_sd, function(x, y) rnorm(1, x, y))

  list(
    alpha = runif(1, -alpha_lim, alpha_lim),
    betas = matrix(X, ncol = 1)
  )
}


#' transform param list into single row table
#' with alpha and B having names corresponding to data
#'
#' @param params
#' @param col_names
#' @keywords internal
params_as_tibble <- function(params, col_names) {
  
  matrix(c(params$alpha, params$betas), nrow = 1) %>%
    as_tibble() %>%
    set_names(c("real_alpha", col_names %>% tail(-1)))
  
} 


#' create data with linear relationship between y and x
#'
#' @param alpha
#' @param betas
#' @param nrows
#' @param sigma
#' 
#' @keywords internal
generate_data <- function(alpha, betas, nrows, sigma = 1) {

  p <- length(betas) / 2
  
  X1 <- matrix(
    runif(nrows * p, min = 0, max = 100),
    nrow = nrows,
    ncol = p
  ) 
  
  colnames(X1) <- str_c("real", 1:p, sep = "_")
  
  X2 <- mapply(
    function(num_cat, n) {
      sample(1:num_cat, n, replace = TRUE)
    },
    num_cat = 2:(p+1),
    n = nrows
  )
  colnames(X2) <- str_c("int", 1:p, sep = "_")
  
  # eventually add categorical values
  #
  # X3 <- mapply(
  #   function(num_cat, n) {
  #     as.factor(sample(letters[1:num_cat], n, replace = TRUE))
  #   },
  #   num_cat = 2:(p+1),
  #   n = nrows
  # )
  # colnames(X3) <- str_c("text", 1:p, sep = "_")
  
  X <- cbind(X1, X2)
  Y <- alpha + X %*% betas + rnorm(nrows, 0, sigma)
  
  cbind(Y, X) %>%
    as_tibble() %>%
    rename_all(tolower) %>%
    rename(real_y = v1)
  
}


