context("test-lm-robust-model.R")

lm_robust_data <- readRDS("../testdata/lm_robust.Rds")

pred_types <- c(
  "tidy_x"
  , "none"
  , "se_fit"
  , "conf"
  , "pred"
)

se_types <- c(
  "HC0",
  "HC1",
  "stata",
  "classical"
)


build_models <- function(se_type, df) {
  
  fn1 <- function(se_type, df) {
    
    eqn1 <- mpg ~ cyl + disp + hp + wt
    eqn2 <- mpg ~ 0 + cyl + disp + hp + wt
    
    x1  <- oomlm_robust(eqn1, se_type = se_type)
    x1  <- fit(x1,  data = df)
    x2  <- oomlm_robust(eqn2, se_type = se_type)
    x2  <- fit(x2,  data = df)
    x3  <- oomlm_robust(eqn1, weights = ~w, se_type = se_type)
    x3  <- fit(x3,  data = df)
    x4  <- oomlm_robust(eqn2, weights = ~w, se_type = se_type)
    x4  <- fit(x4,  data = df)
    
    list(x1, x2, x3, x4)
    
  }
  
  models <- fn1(se_type, df)
  
  fn2 <- function(x) {
    px1 <- predict(x, df)
    px2 <- predict(x, df, se_fit = TRUE)
    px3 <- predict(x, df, se_fit = TRUE, interval = "confidence")
    px4 <- predict(x, df, se_fit = TRUE, interval = "prediction")
    
    list(
      tidy_x  = tidy(x), 
      none    = px1,
      se_fit  = px2,
      conf    = px3,
      pred    = px4
    )
  }
  
  lapply(models, fn2)
  
}


set.seed(42)
df       <- mtcars
w        <- runif(nrow(mtcars))
df['w']  <- w / sum(w)

oomlm_robust_data <- lapply(se_types, build_models, df = df)
names(oomlm_robust_data) <- se_types


expect_attr_equal <- function(se_type) {
  
  se_type <- "HC0"
  
  # y <- lm_robust_data[[se_type]][[1]][["tidy_x"]]
  # x <- oomlm_robust_data[[se_type]][[1]][["tidy_x"]]
  # expect_equal(as.data.frame(x), y[, 1:7])
  # 
  # y <- lm_robust_data[[se_type]][[1]][["none"]]
  # x <- oomlm_robust_data[[se_type]][[1]][["none"]]
  # expect_equal(y, x)
  # 
  # y <- lm_robust_data[[se_type]][[1]][["se_fit"]]
  # x <- oomlm_robust_data[[se_type]][[1]][["se_fit"]]
  # expect_equal(y, head(x, 2))
  # 
  # y <- lm_robust_data[[se_type]][[1]][["conf"]]
  # x <- oomlm_robust_data[[se_type]][[1]][["conf"]]
  # colnames(x$fit) <- colnames(y$fit)
  # rownames(x$fit) <- NULL
  # expect_equal(y, head(x, 2))
  # 
  # y <- lm_robust_data[[se_type]][[1]][["pred"]]
  # x <- oomlm_robust_data[[se_type]][[1]][["pred"]]
  # colnames(x$fit) <- colnames(y$fit)
  # rownames(x$fit) <- NULL
  # expect_equal(y, head(x, 2))
  
}


test_that("robust oomlm HC0", {
  expect_attr_equal("HC0")
})

test_that("robust oomlm HC1", {
  expect_attr_equal("HC1")
})

test_that("robust oomlm stata", {
  expect_attr_equal("stata")
})

test_that("robust oomlm classical", {
  expect_attr_equal("classical")
})

test_that("bad se_type input", {

  df <- mtcars
  f  <- mpg ~ cyl + disp + hp + wt

  expect_error(oomlm_robust(formula = f, se_type = "wut?"))

})
