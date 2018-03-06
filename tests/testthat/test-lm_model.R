context("test-lm_model.R")


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
  
  expect_summary_equal(
    summary(y, correlation = TRUE),
    summary(x, correlation = TRUE)
  )

})


test_that("weighted coef.online_lm works", {
  
  df      <- mtcars
  w       <- runif(nrow(mtcars))
  df['w'] <- w / sum(w)
  
  f <- mpg ~ cyl + disp + hp + wt
  y <- lm(f, data = df, weights = w)
  x <- online_lm(df, f, weights = ~w)
  
  expect_equal(coef(x), coef(y))
})


test_that("weighted vcov.online_lm works", {
  
  df      <- mtcars
  w       <- runif(nrow(mtcars))
  df['w'] <- w / sum(w)
  
  f <- mpg ~ cyl + disp + hp + wt
  y <- lm(f, data = df, weights = w)
  x <- online_lm(df, f, weights = ~w)
  
  expect_equal(vcov(x), vcov(y))
  
})


test_that("weighted summary.online_lm works", {
  
  df      <- mtcars
  w       <- runif(nrow(mtcars))
  df['w'] <- w / sum(w)
  
  f <- mpg ~ cyl + disp + hp + wt
  y <- lm(f, data = df, weights = w)
  x <- online_lm(df, f, weights = ~w)
  
  expect_summary_equal(
    summary(y, correlation = TRUE),
    summary(x, correlation = TRUE)
  )
  
})


test_that("no intercept, weighted coef.online_lm works", {
  
  df      <- mtcars
  w       <- runif(nrow(mtcars))
  df['w'] <- w / sum(w)
  
  f <- mpg ~ 0 + cyl + disp + hp + wt
  
  y <- lm(f, data = df, weights = w)
  x <- online_lm(df, f, weights = ~w)
  
  expect_equal(coef(x), coef(y))
  
})


test_that("no intercept, weighted vcov.online_lm works", {
  
  df      <- mtcars
  w       <- runif(nrow(mtcars))
  df['w'] <- w / sum(w)
  
  f <- mpg ~ 0 + cyl + disp + hp + wt
  
  y <- lm(f, data = df, weights = w)
  x <- online_lm(df, f, weights = ~w)
  
  expect_equal(vcov(x), vcov(y))
  
})


test_that("no intercept, weighted summary.online_lm works", {
  
  df      <- mtcars
  w       <- runif(nrow(mtcars))
  df['w'] <- w / sum(w)
  
  f <- mpg ~ 0 + cyl + disp + hp + wt
  
  y <- lm(f, data = df)
  x <- online_lm(df, f)
  
  expect_summary_equal(
    summary(y, correlation = TRUE),
    summary(x, correlation = TRUE)
  )
  
})
