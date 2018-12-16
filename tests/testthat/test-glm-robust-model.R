context("test-glm-robust-model.R")


iter_model <- function(df, eqn, weights = NULL) {
  
  x <- ploom::oomglm(formula = eqn, weights = weights)
  iter_weight(x, df, max_iter = 8)
  
}


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


test_that("updating oomglm", {
  
  f <- mpg ~ cyl + disp + hp + wt
  y <- glm(f, data = mtcars)
  x <- iter_model(mtcars, f)
  
  expect_equal(coef(x), coef(y))
  expect_equal(vcov(x), vcov(y))
  expect_summary_equal(
    summary(y, correlation = TRUE),
    summary(x, correlation = TRUE)
  )
  
  expect_equal(
    predict(y, mtcars),
    drop(predict(x, mtcars))
  )
  
})


test_that("weighted updating oomglm", {
  
  df      <- mtcars
  w       <- runif(nrow(mtcars))
  df['w'] <- w / sum(w)
  
  f <- mpg ~ cyl + disp + hp + wt
  y <- glm(f, data = df, weights = w)
  x <- iter_model(df, f, weights = ~w)
  
  expect_equal(coef(x), coef(y))
  expect_equal(vcov(x), vcov(y))
  expect_summary_equal(
    summary(y, correlation = TRUE),
    summary(x, correlation = TRUE)
  )
  
  expect_equal(
    predict(y, mtcars),
    drop(predict(x, mtcars))
  )
  
})


test_that("updating oomglm without intercept", {
  
  df <- mtcars
  f  <- mpg ~ 0 + cyl + disp + hp + wt
  
  y <- glm(f, data = df)
  x <- iter_model(df, f)
  
  expect_equal(coef(x), coef(y))
  expect_equal(vcov(x), vcov(y))
  expect_summary_equal(
    summary(y, correlation = TRUE),
    summary(x, correlation = TRUE)
  )
  
  expect_equal(
    predict(y, mtcars),
    drop(predict(x, mtcars))
  )
  
})


test_that("weighted updating oomglm without intercept", {
  
  df      <- mtcars
  w       <- runif(nrow(mtcars))
  df['w'] <- w / sum(w)
  
  f <- mpg ~ 0 + cyl + disp + hp + wt
  
  y <- glm(f, data = df, weights = w)
  x <- iter_model(df, f, weights = ~w)
  
  expect_equal(vcov(x), vcov(y))
  expect_summary_equal(
    summary(y, correlation = TRUE),
    summary(x, correlation = TRUE)
  )
  
  expect_equal(
    predict(y, mtcars),
    drop(predict(x, mtcars))
  )
  
})
