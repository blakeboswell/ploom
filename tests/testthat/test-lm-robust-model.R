context("test-lm-model.R")

robust_models <- readRDS("../testdata/robust-models.Rds")
se_types      <- c("classical", "HC0", "HC1", "stata")


iter_model <- function(df, eqn, se_type, weights = NULL) {


  if(is.null(weights)){
    x <- ploom::oomlm_robust(
      formula = eqn,
      data    = df[1, ],
      se_type = se_type)
  } else {
    x <- ploom::oomlm_robust(
      formula = eqn,
      data    = df[1, ],
      se_type = se_type,
      weights = weights)
  }

  for(i in 2:nrow(df)) {
      x <- update(x, df[i, ])
  }

  x
}


expect_summary_equal <- function(sy, sx) {

  expect_equal(sy$adj.r.squared, sx$adj.r.squared)
  # expect_equal(sy$aliased, sx$aliased)
  expect_equal(
    sy$coefficients[, 1:ncol(sx$coefficients)],
    sx$coefficients)
  # expect_equal(sy$correlation, sx$correlation)
  # expect_equal(sy$cov.unscaled, sx$cov.unscaled)
  # expect_equal(sy$df, sx$df)
  expect_equal(sy$fstatistic, sx$fstatistic)
  expect_equal(sy$r.squared, sx$r.squared)
  # expect_equal(sy$sigma, sx$sigma)
  # expect_equal(sy$terms, sx$names)

}


test_that("robust oomlm", {

  df <- mtcars
  f  <- mpg ~ cyl + disp + hp + wt
  
  for(se_type in se_types) {
    
    y <- robust_models[["regular"]][[se_type]]
    x <- iter_model(df, f, se_type = se_type)
    
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
    
  }
  
})


test_that("weighted robust oomlm", {

  df      <- mtcars
  set.seed(42)
  w       <- runif(nrow(mtcars))
  df['w'] <- w / sum(w)
  
  f <- mpg ~ cyl + disp + hp + wt
  
  for(se_type in se_types) {
    
    y <- robust_models[["weights"]][[se_type]]
    x <- iter_model(df, f, weights = ~w, se_type = se_type)
  
    expect_equal(coef(x), coef(y))
    expect_equal(vcov(x), vcov(y))
    expect_summary_equal(
      summary(y, correlation = TRUE),
      summary(x, correlation = TRUE)
    )
  
    expect_equal(
      predict(y, df),
      drop(predict(x, df))
    )
    
  }
})
 

test_that("updating oomlm without intercept", {

  df <- mtcars
  f  <- mpg ~ 0 + cyl + disp + hp + wt
  
  for(se_type in se_types) {
    
    y <- robust_models[["noint"]][[se_type]]
    x <- iter_model(df, f, se_type = se_type)
  
    expect_equal(coef(x), coef(y))
    expect_equal(vcov(x), vcov(y))
    expect_summary_equal(
      summary(y, correlation = TRUE),
      summary(x, correlation = TRUE)
    )
  
    expect_equal(
      predict(y, df),
      drop(predict(x, df))
    )
    
  }

})


test_that("weighted updating oomlm without intercept", {

  df      <- mtcars
  set.seed(42)
  w       <- runif(nrow(mtcars))
  df['w'] <- w / sum(w)

  f <- mpg ~ 0 + cyl + disp + hp + wt

  for(se_type in se_types) {
    
    y <- robust_models[["weights_noint"]][[se_type]]
    x <- iter_model(df, f, weights = ~w, se_type = se_type)
  
    expect_equal(vcov(x), vcov(y))
    expect_summary_equal(
      summary(y, correlation = TRUE),
      summary(x, correlation = TRUE)
    )
  
    expect_equal(
      predict(y, df),
      drop(predict(x, df))
    )
  }

})
