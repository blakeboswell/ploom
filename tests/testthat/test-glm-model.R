context("test-glm-model.R")


iter_model <- function(df, eqn, weights = NULL) {
  x <- oomglm(formula = eqn, weights = weights)
  fit(x, df, times = 8)
}


expect_attr_equal <- function(x, y, df) {

  expect_equal(family(y), family(x))
  expect_equal(coef(x), coef(y))
  expect_equal(vcov(x), vcov(y))
  
  sy <- summary(y, correlation = TRUE)
  sx <- summary(x, correlation = TRUE)
  
  expect_equal(sy$terms, sx$terms)
  expect_equal(deviance(sy), deviance(sx))
  expect_equal(sy$df.residual, sx$df.residual)
  expect_equal(sy$df.null, sx$df.null)
  expect_equal(sy$iter, sx$iter)
  expect_equal(sy$coefficients, sx$coefficients)
  expect_equal(sy$aliased, sx$aliased)
  expect_equal(sy$df, sx$df)
  expect_equal(sy$correlation, sx$correlation)
  expect_equal(sy$cov.unscaled, sx$cov.unscaled)

  yy        <- as.vector(predict(y, df, type = "response"))
  xy        <- predict(x, df, type = "response")$.pred
  expect_equal(yy, xy)
  
  yy        <- as.vector(predict(y, df, type = "link"))
  xy        <- predict(x, df, type = "link")$.pred
  expect_equal(yy, xy)

  yy <- tryCatch({hbroom::augment(y, df)}, error = function(e) { NULL })
  xy <- augment(x, df)
  if(!is.null(yy)) {
    expect_equal(yy$.fitted, xy$.fitted)
    expect_equal(yy$.resid, xy$.resid)
  }
  
  yy <- tryCatch({hbroom::augment(y, df)}, error = function(e) { NULL })
  xy <- augment(x, df, std_error = TRUE)
  if(!is.null(yy)) {
    expect_equal(yy$.fitted, xy$.fitted)
    expect_equal(yy$.resid, xy$.resid)
    expect_equal(yy$.se.fit, xy$.st_error)
  }

  yy <- as.vector(residuals(y))
  xy <- residuals(x, df)$.resid
  expect_equal(yy, xy)
  
  yy <- tryCatch({hbroom::glance(y)}, error = function(e) { NULL })
  xy <- glance(x)
  if(!is.null(yy)) {
    expect_equal(
      as.matrix(unclass(yy[names(xy)])),
      as.matrix(unclass(xy))
    )  
  }
  
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


test_that("weighted oomglm with zero weight", {

  df      <- mtcars
  w       <- runif(nrow(mtcars))
  w[4:7]  <- 0.0
  df['w'] <- w / sum(w)

  f <- mpg ~ cyl + disp + hp + wt
  y <- glm(f, data = df, weights = w)
  x <- fit(oomglm(f, weights = ~w),
           oomdata_tbl(df, chunk_size = 2))

  expect_attr_equal(x, y, df)
})
