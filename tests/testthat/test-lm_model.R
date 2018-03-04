context("test-lm_model.R")

test_that("coef.online_lm works", {
  
  f <- mpg ~ cyl + disp + hp + wt
  y <- lm(f, data = mtcars)
  x <- online_lm(mtcars, f)
  
  expect_equal(coef(x), coef(y))
})

test_that("vcov.online_lm works", {
  
  f <- mpg ~ cyl + disp + hp + wt
  y <- lm(f, data = mtcars)
  x <- online_lm(mtcars, f)
  
  expect_equal(vcov(x), vcov(y))
})


test_that("summary.online_lm works", {
  
  f <- mpg ~ cyl + disp + hp + wt
  y <- lm(f, data = mtcars)
  x <- online_lm(mtcars, f)
  
  sy <- summary(y, correlation = TRUE)
  sx <- summary(x, correlation = TRUE)
  
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
  
})


