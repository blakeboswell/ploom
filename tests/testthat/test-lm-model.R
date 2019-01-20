context("test-lm-model.R")


iter_model <- function(df, eqn, weights = NULL) {

  if(is.null(weights)){
    x <- ploom::oomlm(formula = eqn, data = df[1, ])
  }
  else {
    x <- ploom::oomlm(formula = eqn, data = df[1, ], weights = weights)
  }

  for(i in 2:nrow(df)) {
    x <- update(x, df[i, ])
  }

  x
}


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

expect_attr_equal <- function(x, y) {
  
  expect_equal(coef(x), coef(y))
  expect_equal(vcov(x), vcov(y))
  
  yy        <- predict(y, mtcars)
  names(yy) <- NULL
  xy        <- drop(predict(x, mtcars))
  names(xy) <- NULL
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
    quiet(summary(x, correlation = TRUE)),
    summary(x, correlation = TRUE)
  )
  
  
}



test_that("updating oomlm", {

  f <- mpg ~ cyl + disp + hp + wt
  y <- lm(f, data = mtcars)
  x <- iter_model(mtcars, f)
  
  expect_summary_equal(
    summary(y, correlation = TRUE),
    summary(x, correlation = TRUE)
  )
  
  expect_attr_equal(x, y)
  
})


test_that("weighted updating oomlm", {

  df      <- mtcars
  w       <- runif(nrow(mtcars))
  df['w'] <- w / sum(w)

  f <- mpg ~ cyl + disp + hp + wt
  y <- lm(f, data = df, weights = w)
  x <- iter_model(df, f, weights = ~w)

  expect_summary_equal(
    summary(y, correlation = TRUE),
    summary(x, correlation = TRUE)
  )
  
  expect_attr_equal(x, y)
  
})


test_that("updating oomlm without intercept", {

  df <- mtcars
  f  <- mpg ~ 0 + cyl + disp + hp + wt

  y <- lm(f, data = df)
  x <- iter_model(df, f)

  expect_summary_equal(
    summary(y, correlation = TRUE),
    summary(x, correlation = TRUE)
  )
  
  expect_attr_equal(x, y)
  
})


test_that("weighted updating oomlm without intercept", {

  df      <- mtcars
  w       <- runif(nrow(mtcars))
  df['w'] <- w / sum(w)

  f <- mpg ~ 0 + cyl + disp + hp + wt

  y <- lm(f, data = df, weights = w)
  x <- iter_model(df, f, weights = ~w)

  expect_summary_equal(
    summary(y, correlation = TRUE),
    summary(x, correlation = TRUE)
  )
  
  expect_attr_equal(x, y)
})
