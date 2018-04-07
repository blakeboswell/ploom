context("test-static-glm_model.R")

expect_summary_equal <- function(sy, sx) {
  
  expect_equal(sy$terms, sx$terms)
  expect_equal(sy$family, sx$family)
  expect_equal(sy$deviance, sx$deviance)
  expect_equal(sy$aic, sx$aic)
  expect_equal(sy$df.residual, sx$df.residual)
  # expect_equal(sy$null.deviance, sx$null.deviance)
  expect_equal(sy$df.null, sx$df.null)
  expect_equal(sy$iter, sx$iter)
  expect_equal(sy$coefficients, sx$coefficients)
  expect_equal(sy$aliased, sx$aliased)
  # expect_equal(sy$dispersion, sx$dispersion)
  expect_equal(sy$df, sx$df)
  expect_equal(sy$correlation, sx$correlation)
  expect_equal(sy$cov.unscaled, sx$cov.unscaled)
  
}


test_that("oomglm", {
  
  f <- mpg ~ cyl + disp + hp + wt
  
  y <- glm(f, data = mtcars)
  x <- reweight_oomglm(oomglm(f), mtcars, num_iterations = 8L)
  
  expect_equal(coef(x), coef(y))
  expect_equal(vcov(x), vcov(y))
  
  expect_summary_equal(
    summary(y, correlation = TRUE),
    summary(x, correlation = TRUE)
  )
  
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
  
  expect_summary_equal(
    summary(y, correlation = TRUE),
    summary(x, correlation = TRUE)
  )
  
})


test_that("oomglm without intercept", {
  
  df <- mtcars
  f  <- mpg ~ 0 + cyl + disp + hp + wt
  
  y <- glm(f, data = df)
  x <- reweight_oomglm(oomglm(f), data = df, num_iterations = 8L)
  
  expect_equal(coef(x), coef(y))
  expect_equal(vcov(x), vcov(y))
  
  expect_summary_equal(
    summary(y, correlation = TRUE),
    summary(x, correlation = TRUE)
  )
  
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
  
  expect_summary_equal(
    summary(y, correlation = TRUE),
    summary(x, correlation = TRUE)
  )
  
})
