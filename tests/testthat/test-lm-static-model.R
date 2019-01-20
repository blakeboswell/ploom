context("test-static-lm-model.R")


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


test_that("oomlm", {

  f <- mpg ~ cyl + disp + hp + wt
  y <- lm(f, data = mtcars)
  x <- oomlm(f, mtcars)

  expect_equal(coef(x), coef(y))
  expect_equal(vcov(x), vcov(y))
  expect_summary_equal(
    summary(y, correlation = TRUE),
    summary(x, correlation = TRUE)
  )
  expect_equal(
    as.matrix(tidy(y)[2:5]),
    as.matrix(tidy(x)[2:5])
  )

})


test_that("weighted oomlm", {

  df      <- mtcars
  w       <- runif(nrow(mtcars))
  df['w'] <- w / sum(w)

  f <- mpg ~ cyl + disp + hp + wt
  y <- lm(f, data = df, weights = w)
  x <- oomlm(f, df, weights = ~w)

  expect_equal(coef(x), coef(y))
  expect_equal(vcov(x), vcov(y))
  expect_summary_equal(
    summary(y, correlation = TRUE),
    summary(x, correlation = TRUE)
  )
  expect_equal(
    as.matrix(tidy(y)[2:5]),
    as.matrix(tidy(x)[2:5])
  )

})


test_that("weighted oomlm with zero weight", {

  df      <- mtcars
  w       <- runif(nrow(mtcars))
  w[4:7]  <- 0.0
  df['w'] <- w / sum(w)

  f <- mpg ~ cyl + disp + hp + wt
  y <- lm(f, data = df, weights = w)
  x <- oomlm(f, df, weights = ~w)

  expect_equal(coef(x), coef(y))
  expect_equal(vcov(x), vcov(y))
  expect_summary_equal(
    summary(y, correlation = TRUE),
    summary(x, correlation = TRUE)
  )
  expect_equal(
    as.matrix(tidy(y)[2:5]),
    as.matrix(tidy(x)[2:5])
  )

})



test_that("oomlm without intercept", {

  df <- mtcars
  f  <- mpg ~ 0 + cyl + disp + hp + wt

  y <- lm(f, data = df)
  x <- oomlm(f, df)

  expect_equal(coef(x), coef(y))
  expect_equal(vcov(x), vcov(y))
  expect_summary_equal(
    summary(y, correlation = TRUE),
    summary(x, correlation = TRUE)
  )
  expect_equal(
    as.matrix(tidy(y)[2:5]),
    as.matrix(tidy(x)[2:5])
  )

})


test_that("weighted oomlm without intercept", {

  df      <- mtcars
  w       <- runif(nrow(mtcars))
  df['w'] <- w / sum(w)

  f <- mpg ~ 0 + cyl + disp + hp + wt

  y <- lm(f, data = df, weights = w)
  x <- oomlm(f, df, weights = ~w)

  expect_equal(vcov(x), vcov(y))
  expect_summary_equal(
    summary(y, correlation = TRUE),
    summary(x, correlation = TRUE)
  )
  expect_equal(
    as.matrix(tidy(y)[2:5]),
    as.matrix(tidy(x)[2:5])
  )

})
