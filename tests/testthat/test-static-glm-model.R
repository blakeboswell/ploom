context("test-static-glm-model.R")

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


test_that("oomglm", {

  f <- mpg ~ cyl + disp + hp + wt

  y <- glm(f, data = mtcars)
  x <- reweight(oomglm(f), mtcars, max_iter = 8L)

  expect_equal(coef(x), coef(y))
  expect_equal(vcov(x), vcov(y))

  expect_summary_equal(
    summary(y, correlation = TRUE),
    summary(x, correlation = TRUE)
  )


  f2 <- mpg ~ cyl
  y2 <- glm(f2, data = mtcars)
  x2 <- reweight(oomglm(f2), mtcars, max_iter = 8L)

  expect_equal(AIC(y) - AIC(y2),
               AIC(x) - AIC(x2))

})


test_that("weighted oomglm", {

  df      <- mtcars
  w       <- runif(nrow(mtcars))
  w       <- w / sum(w)
  df['w'] <- w

  f <- mpg ~ cyl + disp + hp + wt
  y <- glm(f, data = df, weights = w)
  x <- oomglm(f, weights = ~w)
  x <- reweight(x, df, max_iter = 8L)

  expect_equal(coef(x), coef(y))
  expect_equal(vcov(x), vcov(y))

  expect_summary_equal(
    summary(y, correlation = TRUE),
    summary(x, correlation = TRUE)
  )

  f2 <- mpg ~ cyl
  y2 <- glm(f2, data = mtcars, weights = w)
  x2 <- oomglm(f2, weights = ~w)
  x2 <- reweight(x2, mtcars, max_iter = 8L)

  expect_equal(AIC(y) - AIC(y2),
               AIC(x) - AIC(x2))

})



test_that("weighted oomglm with zero weight", {

  df      <- mtcars
  w       <- runif(nrow(mtcars))
  w[4:7]  <- 0.0
  w       <- w / sum(w)
  df['w'] <- w

  f <- mpg ~ cyl + disp + hp + wt
  y <- glm(f, data = df, weights = w)
  x <- oomglm(f, weights = ~w)
  x <- reweight(x, df, max_iter = 8L)

  expect_equal(coef(x), coef(y))
  expect_equal(vcov(x), vcov(y))

  expect_summary_equal(
    summary(y, correlation = TRUE),
    summary(x, correlation = TRUE)
  )

  # glm returns AIC = Inf when zero weights
  # are included ...
  # f2 <- mpg ~ cyl
  # y2 <- glm(f2, data = mtcars, weights = w)
  # x2 <- oomglm(f2, weights = ~w)
  # x2 <- reweight(x2, mtcars, max_iter = 8L)
  #
  # expect_equal(AIC(y) - AIC(y2),
  #              AIC(x) - AIC(x2))

})


test_that("oomglm without intercept", {

  df <- mtcars
  f  <- mpg ~ 0 + cyl + disp + hp + wt

  y <- glm(f, data = df)
  x <- reweight(oomglm(f), data = df, max_iter = 8L)

  expect_equal(coef(x), coef(y))
  expect_equal(vcov(x), vcov(y))

  expect_summary_equal(
    summary(y, correlation = TRUE),
    summary(x, correlation = TRUE)
  )

  f2 <- mpg ~ 0 + cyl
  y2 <- glm(f2, data = mtcars)
  x2 <- reweight(oomglm(f2), mtcars, max_iter = 8L)

  expect_equal(AIC(y) - AIC(y2),
               AIC(x) - AIC(x2))

})


test_that("weighted oomglm without intercept", {

  df      <- mtcars
  w       <- runif(nrow(mtcars))
  w       <- w / sum(w)
  df['w'] <- w

  f <- mpg ~ 0 + cyl + disp + hp + wt

  y <- glm(f, data = df, weights = w)
  x <- oomglm(f, weights = ~w)
  x <- reweight(x, data = df, max_iter = 8L)

  expect_equal(coef(x), coef(y))
  expect_equal(vcov(x), vcov(y))

  expect_summary_equal(
    summary(y, correlation = TRUE),
    summary(x, correlation = TRUE)
  )

  f2 <- mpg ~ 0 + cyl
  y2 <- glm(f2, data = mtcars, weights = w)
  x2 <- oomglm(f2, weights = ~w)
  x2 <- reweight(x2, mtcars, max_iter = 8L)

  expect_equal(AIC(y) - AIC(y2),
               AIC(x) - AIC(x2))

})
