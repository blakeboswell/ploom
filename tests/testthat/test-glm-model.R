context("test-glm-model.R")


iter_model <- function(df, eqn, weights = NULL) {
  x <- oomglm(formula = eqn, weights = weights)
  fit(x, df, times = 8)
}


expect_summary_equal <- function(sy, sx) {

  expect_equal(sy$terms, sx$terms)
  expect_equal(sy$family, sx$family)
  expect_equal(sy$deviance, sx$deviance)
  expect_equal(sy$df.residual, sx$df.residual)
  expect_equal(sy$df.null, sx$df.null)
  expect_equal(sy$iter, sx$iter)
  expect_equal(sy$coefficients, sx$coefficients)
  expect_equal(sy$aliased, sx$aliased)
  expect_equal(sy$df, sx$df)
  expect_equal(sy$correlation, sx$correlation)
  expect_equal(sy$cov.unscaled, sx$cov.unscaled)
  
  # not impl
  # expect_equal(sy$null.deviance, sx$null.deviance)
  # expect_equal(sy$dispersion, sx$dispersion)
}


expect_attr_equal <- function(x, y, df) {

  expect_summary_equal(
    summary(y, correlation = TRUE),
    summary(x, correlation = TRUE)
  )
  
  expect_equal(coef(x), coef(y))
  expect_equal(vcov(x), vcov(y))

  # yy        <- predict(y, df)
  # xy        <- predict(x, df)
  # expect_equal(yy, xy)
  # 
  # yy        <- predict(y, df, se.fit = TRUE)
  # xy        <- predict(x, df, se_fit = TRUE)
  # names(xy) <- names(yy)
  # expect_equal(yy, xy)
  # 
  # yy <- residuals(y)
  # xy <- oomglm_residuals(x, df, type = "deviance")
  # expect_equal(yy, xy)

  expect_equal(
    as.matrix(broom::tidy(y)[2:5]),
    as.matrix(tidy(x)[2:5])
  )

  quiet <- function(x) { 
    sink(tempfile()) 
    on.exit(sink()) 
    invisible(force(x)) 
  } 
  
  expect_equal(quiet(print(x)), x)
  expect_equal(quiet(print(summary(x))), summary(x))
  expect_equal(
    quiet(print(summary(x, correlation = TRUE))),
    summary(x, correlation = TRUE)
  )
  
}


test_that("updating oomglm", {

  f <- mpg ~ cyl + disp + hp + wt
  y <- glm(f, data = mtcars)
  x <- iter_model(mtcars, f)

  expect_attr_equal(x, y, mtcars)

})

test_that("weighted updating oomglm", {

  df      <- mtcars
  w       <- runif(nrow(mtcars))
  df['w'] <- w / sum(w)

  f <- mpg ~ cyl + disp + hp + wt
  y <- glm(f, data = df, weights = w)
  x <- iter_model(df, f, weights = ~w)

  expect_attr_equal(x, y, df)

})

test_that("updating oomglm without intercept", {

  df <- mtcars
  f  <- mpg ~ 0 + cyl + disp + hp + wt

  y <- glm(f, data = df)
  x <- iter_model(df, f)

  expect_attr_equal(x, y, df)

})

test_that("weighted updating oomglm without intercept", {

  df      <- mtcars
  w       <- runif(nrow(mtcars))
  df['w'] <- w / sum(w)

  f <- mpg ~ 0 + cyl + disp + hp + wt

  y <- glm(f, data = df, weights = w)
  x <- iter_model(df, f, weights = ~w)

  expect_attr_equal(x, y, df)

})
