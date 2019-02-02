context("test-lm-model.R")


iter_model <- function(df, eqn, weights = NULL) {
  x <- oomlm(formula = eqn, weights = weights)
  for(i in 1:nrow(df)) {
    x <- fit(x, df[i, ])
  }
  x
}


expect_attr_equal <- function(x, y, df) {
  
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
  expect_equal(coef(x), coef(y))
  expect_equal(vcov(x), vcov(y))
  
  yy        <- as.vector(predict(y, df))
  xy        <- predict(x, df)$.pred
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
  
  yy <- predict(y, mtcars, se.fit = TRUE, interval = "confidence")
  yy <- matrix(yy$fit, ncol = 3)
  xy <- predict(x, mtcars, std_error = TRUE, interval = "confidence")
  xy <- cbind(xy$.pred, xy$.pred_lower, xy$.pred_upper)
  yy <- predict(y, mtcars, se.fit = TRUE, interval = "prediction")
  yy <- matrix(yy$fit, ncol = 3)
  xy <- predict(x, mtcars, std_error = TRUE, interval = "prediction")
  xy <- cbind(xy$.pred, xy$.pred_lower, xy$.pred_upper)

  # 
  # yy <- predict(y, df, se.fit = TRUE, interval = "confidence")
  # xy <- predict(x, df, se_fit = TRUE, interval = "confidence")
  # colnames(xy$fit) <- colnames(yy$fit)
  # expect_equal(yy, xy)
  # 
  # yy <- predict(y, df, se.fit = TRUE, interval = "prediction")
  # xy <- predict(x, df, se_fit = TRUE, interval = "prediction")
  # colnames(xy$fit) <- colnames(yy$fit)
  # expect_equal(yy, xy)
  # 
  
  yy <- as.vector(residuals(y))
  xy <- residuals(x, df)$.resid
  expect_equal(yy, xy)
  xy <- resid(x, df)$.resid
  expect_equal(yy, xy)
  
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



test_that("updating oomlm", {

  f <- mpg ~ cyl + disp + hp + wt
  y <- lm(f, data = mtcars)
  x <- iter_model(mtcars, f)
  
  expect_attr_equal(x, y, mtcars)
  
})


test_that("weighted updating oomlm", {

  df      <- mtcars
  w       <- runif(nrow(mtcars))
  df['w'] <- w / sum(w)

  f <- mpg ~ cyl + disp + hp + wt
  y <- lm(f, data = df, weights = w)
  x <- iter_model(df, f, weights = ~w)

  expect_attr_equal(x, y, df)

})


test_that("updating oomlm without intercept", {

  df <- mtcars
  f  <- mpg ~ 0 + cyl + disp + hp + wt

  y <- lm(f, data = df)
  x <- iter_model(df, f)

  expect_attr_equal(x, y, df)

})


test_that("weighted updating oomlm without intercept", {

  df      <- mtcars
  w       <- runif(nrow(mtcars))
  df['w'] <- w / sum(w)

  f <- mpg ~ 0 + cyl + disp + hp + wt

  y <- lm(f, data = df, weights = w)
  x <- iter_model(df, f, weights = ~w)

  expect_attr_equal(x, y, df)

})



test_that("oomlm", {
  
  df <- mtcars
  f  <- mpg ~ cyl + disp + hp + wt
  y  <- lm(f, data = df)
  x  <- fit(oomlm(f),
            oomdata_tbl(df, chunk_size = 2))

  expect_attr_equal(x, y, df)

})


test_that("weighted oomlm", {

  df      <- mtcars
  w       <- runif(nrow(mtcars))
  df['w'] <- w / sum(w)

  f <- mpg ~ cyl + disp + hp + wt
  y <- lm(f, data = df, weights = w)
  x <- fit(oomlm(f, weights = ~w),
           oomdata_tbl(df, chunk_size = 2))

  expect_attr_equal(x, y, df)

})


test_that("weighted oomlm with zero weight", {

  df      <- mtcars
  w       <- runif(nrow(mtcars))
  w[4:7]  <- 0.0
  df['w'] <- w / sum(w)

  f <- mpg ~ cyl + disp + hp + wt
  y <- lm(f, data = df, weights = w)
  x <- fit(oomlm(f, weights = ~w),
           oomdata_tbl(df, chunk_size = 2))

  expect_attr_equal(x, y, df)
})



test_that("oomlm without intercept", {

  df <- mtcars
  f  <- mpg ~ 0 + cyl + disp + hp + wt

  y <- lm(f, data = df)
  x <- fit(oomlm(f),
           oomdata_tbl(df, chunk_size = 2))

  expect_attr_equal(x, y, df)

})


test_that("weighted oomlm without intercept", {

  df      <- mtcars
  w       <- runif(nrow(mtcars))
  df['w'] <- w / sum(w)

  f <- mpg ~ 0 + cyl + disp + hp + wt

  y <- lm(f, data = df, weights = w)
  x <- fit(oomlm(f, weights = ~w),
           oomdata_tbl(df, chunk_size = 2))

  expect_attr_equal(x, y, df)

})

