context("test-updating-lm_model.R")


iter_model <- function(df, eqn, weights = NULL) {
  
  
  if(is.null(weights)){
    x <- ploom::oomlm(df[1, ],
                          formula = eqn)
  }
  else {
    x <- ploom::oomlm(df[1, ],
                          formula = eqn,
                          weights = weights)
  }
  
  for(i in 2:nrow(df)) {
      x <- update_oomlm(df[i, ], x)
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


test_that("updating oomlm", {
  
  f <- mpg ~ cyl + disp + hp + wt
  y <- lm(f, data = mtcars)
  x <- iter_model(mtcars, f)
  
  expect_equal(coef(x), coef(y))
  expect_equal(vcov(x), vcov(y))
  expect_summary_equal(
    summary(y, correlation = TRUE),
    summary(x, correlation = TRUE)
  )
  
})


test_that("weighted updating oomlm", {

  df      <- mtcars
  w       <- runif(nrow(mtcars))
  df['w'] <- w / sum(w)

  f <- mpg ~ cyl + disp + hp + wt
  y <- lm(f, data = df, weights = w)
  x <- iter_model(df, f, weights = ~w)

  expect_equal(coef(x), coef(y))
  expect_equal(vcov(x), vcov(y))
  expect_summary_equal(
    summary(y, correlation = TRUE),
    summary(x, correlation = TRUE)
  )

})


test_that("updating oomlm without intercept", {
  
  df <- mtcars
  f  <- mpg ~ 0 + cyl + disp + hp + wt
  
  y <- lm(f, data = df)
  x <- iter_model(df, f)
  
  expect_equal(coef(x), coef(y))
  expect_equal(vcov(x), vcov(y))
  expect_summary_equal(
    summary(y, correlation = TRUE),
    summary(x, correlation = TRUE)
  )
  
})


test_that("weighted updating oomlm without intercept", {

  df      <- mtcars
  w       <- runif(nrow(mtcars))
  df['w'] <- w / sum(w)

  f <- mpg ~ 0 + cyl + disp + hp + wt

  y <- lm(f, data = df, weights = w)
  x <- iter_model(df, f, weights = ~w)

  expect_equal(vcov(x), vcov(y))
  expect_summary_equal(
    summary(y, correlation = TRUE),
    summary(x, correlation = TRUE)
  )

})
