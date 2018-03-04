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



