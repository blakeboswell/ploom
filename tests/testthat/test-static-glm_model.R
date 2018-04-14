context("test-static-glm_model.R")

expect_summary_equal <- function(sy, sx) {
  
  expect_equal(sy$terms, sx$terms)
  expect_equal(sy$family, sx$family)
  expect_equal(sy$deviance, sx$deviance)
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


test_that("yglm", {
  
  f <- mpg ~ cyl + disp + hp + wt
  
  y <- glm(f, data = mtcars)
  x <- reweight(yglm(f), mtcars, num_iterations = 8L)
  
  expect_equal(coef(x), coef(y))
  expect_equal(vcov(x), vcov(y))
  
  expect_summary_equal(
    summary(y, correlation = TRUE),
    summary(x, correlation = TRUE)
  )
  
  
  f2 <- mpg ~ cyl
  y2 <- glm(f2, data = mtcars)
  x2 <- reweight(yglm(f2), mtcars, num_iterations = 8L)
  
  expect_equal(AIC(y) - AIC(y2),
               AIC(x) - AIC(x2))
  
})


test_that("weighted yglm", {
  
  df      <- mtcars
  w       <- runif(nrow(mtcars))
  w       <- w / sum(w)
  df['w'] <- w
  
  f <- mpg ~ cyl + disp + hp + wt
  y <- glm(f, data = df, weights = w)
  x <- yglm(f, weights = ~w)
  x <- reweight(x, df, num_iterations = 8L)
  
  expect_equal(coef(x), coef(y))
  expect_equal(vcov(x), vcov(y))
  
  expect_summary_equal(
    summary(y, correlation = TRUE),
    summary(x, correlation = TRUE)
  )
  
  f2 <- mpg ~ cyl
  y2 <- glm(f2, data = mtcars, weights = w)
  x2 <- yglm(f2, weights = ~w)
  x2 <- reweight(x2, mtcars, num_iterations = 8L)
  
  expect_equal(AIC(y) - AIC(y2),
               AIC(x) - AIC(x2))
  
})


test_that("yglm without intercept", {
  
  df <- mtcars
  f  <- mpg ~ 0 + cyl + disp + hp + wt
  
  y <- glm(f, data = df)
  x <- reweight(yglm(f), data = df, num_iterations = 8L)
  
  expect_equal(coef(x), coef(y))
  expect_equal(vcov(x), vcov(y))
  
  expect_summary_equal(
    summary(y, correlation = TRUE),
    summary(x, correlation = TRUE)
  )
  
  f2 <- mpg ~ 0 + cyl
  y2 <- glm(f2, data = mtcars)
  x2 <- reweight(yglm(f2), mtcars, num_iterations = 8L)
  
  expect_equal(AIC(y) - AIC(y2),
               AIC(x) - AIC(x2))
  
})


test_that("weighted yglm without intercept", {
  
  df      <- mtcars
  w       <- runif(nrow(mtcars))
  w       <- w / sum(w)
  df['w'] <- w
  
  f <- mpg ~ 0 + cyl + disp + hp + wt
  
  y <- glm(f, data = df, weights = w)
  x <- yglm(f, weights = ~w)
  x <- reweight(x, data = df, num_iterations = 8L)
  
  expect_equal(coef(x), coef(y))
  expect_equal(vcov(x), vcov(y))
  
  expect_summary_equal(
    summary(y, correlation = TRUE),
    summary(x, correlation = TRUE)
  )
  
  f2 <- mpg ~ 0 + cyl
  y2 <- glm(f2, data = mtcars, weights = w)
  x2 <- yglm(f2, weights = ~w)
  x2 <- reweight(x2, mtcars, num_iterations = 8L)
  
  expect_equal(AIC(y) - AIC(y2),
               AIC(x) - AIC(x2))
  
})
