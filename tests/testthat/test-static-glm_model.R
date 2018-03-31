context("test-static-glm_model.R")

expect_summary_equal <- function(sy, sx) {
  
  expect_equal(sy$adj.r.squared, sx$adj.r.squared)
  expect_equal(sy$aliased, sx$aliased)
  expect_equal(sy$coefficients, sx$coefficients)
  expect_equal(sy$correlation, sx$correlation)
  expect_equal(sy$cov.unscaled, sx$cov.unscaled)
  expect_equal(sy$df, sx$df)
  expect_equal(sy$fstatistic, sx$fstatistic)
  expect_equal(sy$r.squared, sx$r.squared)
  expect_equal(sy$sigma, sx$sigma)
  expect_equal(sy$terms, sx$terms)
  
}


test_that("oomglm", {
  
  f <- mpg ~ cyl + disp + hp + wt
  
  y <- glm(f, data = mtcars)
  x <- reweight_oomglm(oomglm(f), mtcars, num_iterations = 8L)
  
  expect_equal(coef(x), coef(y))
  expect_equal(vcov(x), vcov(y))
  
  # expect_summary_equal(
  #   summary(y, correlation = TRUE),
  #   summary(x, correlation = TRUE)
  # )
  
})


test_that("weighted oomglm", {
  
  df      <- mtcars
  w       <- runif(nrow(mtcars))
  df['w'] <- w / sum(w)
  
  f <- mpg ~ cyl + disp + hp + wt
  y <- glm(f, data = df, weights = w)
  x <- oomglm(f, weights = ~w)
  x <- reweight_oomglm(x, df, num_iterations = 8L)
  
  expect_equal(coef(x), coef(y))
  expect_equal(vcov(x), vcov(y))
  
  # expect_summary_equal(
  #   summary(y, correlation = TRUE),
  #   summary(x, correlation = TRUE)
  # )
  
})


test_that("oomglm without intercept", {
  
  df <- mtcars
  f  <- mpg ~ 0 + cyl + disp + hp + wt
  
  y <- glm(f, data = df)
  x <- reweight_oomglm(oomglm(f), data = df, num_iterations = 8L)
  
  expect_equal(coef(x), coef(y))
  expect_equal(vcov(x), vcov(y))
  
  # expect_summary_equal(
  #   summary(y, correlation = TRUE),
  #   summary(x, correlation = TRUE)
  # )
  
})


test_that("weighted oomglm without intercept", {
  
  df      <- mtcars
  w       <- runif(nrow(mtcars))
  df['w'] <- w / sum(w)
  
  f <- mpg ~ 0 + cyl + disp + hp + wt
  
  y <- glm(f, data = df, weights = w)
  x <- oomglm(f, weights = ~w)
  x <- reweight_oomglm(x, data = df, num_iterations = 8L)
  
  expect_equal(coef(x), coef(y))
  expect_equal(vcov(x), vcov(y))
  
  # expect_summary_equal(
  #   summary(y, correlation = TRUE),
  #   summary(x, correlation = TRUE)
  # )
  
})
