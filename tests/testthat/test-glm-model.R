context("test-glm-model.R")

set.seed(43)

p <- 4
N <- 10^3

params <- linear_params(p)
df     <- generate_data(params$betas, N)
w       <- runif(nrow(df))
df['w'] <- w / sum(w)


make_formula <- function(y, df, noint = FALSE) {

  cols <- colnames(df)
  cols <- cols[cols != 'w']
  p    <- length(cols)

  if(noint) {
    cols <- c("0", cols)
  }

  as.formula(
  paste(y, paste(cols[6:p], collapse = "+"), sep = "~")
  )

}


make_family <- function(y) {
  switch(y,
    real_ygauss = gaussian,
    real_ybin   = binomial,
    real_ypois  = poisson(link = log),
    real_ygamma = Gamma(link = log)
  )
}


iter_model <- function(df, eqn, family, weights = NULL) {
  x <- oomglm(formula = eqn,  family = family, weights = weights)
  fit(x, df, times = 7)
}


expect_attr_equal <- function(x, y, df) {

  expect_equal(family(y), family(x))
  expect_equal(coef(x), coef(y))
  expect_equal(vcov(x), vcov(y), tolerance = 1e-4)
  
  sy <- summary(y, correlation = TRUE)
  sx <- summary(x, correlation = TRUE)
  
  expect_equal(sy$terms, sx$terms)
  expect_equal(deviance(sy), deviance(sx))
  expect_equal(sy$df.residual, sx$df.residual)
  expect_equal(sy$df.null, sx$df.null)
  # expect_equal(sy$iter, sx$iter)
  expect_equal(sy$coefficients, sx$coefficients, tolerance = 1e-4)
  expect_equal(sy$aliased, sx$aliased)
  expect_equal(sy$df, sx$df)
  expect_equal(sy$correlation, sx$correlation, tolerance = 1e-4)
  expect_equal(sy$cov.unscaled, sx$cov.unscaled, tolerance = 1e-4)

  yy        <- as.vector(predict(y, df, type = "response"))
  xy        <- predict(x, df, type = "response")$.pred
  expect_equal(yy, xy)
  yy        <- as.vector(predict(y, df, type = "link"))
  xy        <- predict(x, df, type = "link")$.pred
  expect_equal(yy, xy)
   
  yy <- as.vector(residuals(y, type = "deviance"))
  xy <- as.vector(residuals(x, data = df, type = "deviance")$.resid)
  expect_equal(yy, xy)
  
  yy <- as.vector(residuals(y, type = "pearson"))
  xy <- as.vector(residuals(x, data = df, type = "pearson")$.resid)
  expect_equal(yy, xy)
  
  yy <- as.vector(residuals(y, type = "response"))
  xy <- as.vector(residuals(x, data = df, type = "response")$.resid)
  expect_equal(yy, xy)
  
  yy <- tryCatch(
    {broom::augment(y, df)},
    error = function(e) { NULL }
  )
  xy <- augment(x, df)
  if(!is.null(yy)) {
    expect_equal(yy$.fitted, xy$.fitted)
    expect_equal(yy$.resid, xy$.resid)
  }
 
  yy <- tryCatch({broom::augment(y, df)}, error = function(e) { NULL })
  xy <- augment(x, df, std_error = TRUE)
  if(!is.null(yy)) {
    expect_equal(yy$.fitted, xy$.fitted)
    expect_equal(yy$.resid, xy$.resid)
    expect_equal(yy$.se.fit, xy$.se.fit, tolerance = 1e-4)
  }
  
  yy <- tryCatch(
    {broom::augment(y, df, type.predict = "response")},
    error = function(e) { NULL }
  )
  xy <- augment(x, df, type_predict = "response", std_error = TRUE)
  if(!is.null(yy)) {
    expect_equal(yy$.fitted, xy$.fitted)
    expect_equal(yy$.resid, xy$.resid)
    expect_equal(yy$.se.fit, xy$.se.fit, tolerance = 1e-4)
  }
  
  # yy <- tryCatch({broom::glance(y)}, error = function(e) { NULL })
  # xy <- glance(x)
  # if(!is.null(yy)) {
  #   expect_equal(
  #     as.numeric(unclass(yy[names(xy)])),
  #     as.numeric(as.matrix(unclass(xy)))
  #   )
  # }
  #
  # 
  # expect_equal(
  #   as.matrix(broom::tidy(y)[2:5]),
  #   as.matrix(tidy(x)[2:5])
  # )

}


for(e in colnames(df)[3:3]) {
  
  test_that(paste(e, "oomglm"), {
    eqn <- make_formula(e, df)
    fam <- make_family(e)
    y <- glm(eqn, data = df, family = fam)
    x <- iter_model(df, eqn, family = fam)
    expect_attr_equal(x, y, df)
  })
  
  # test_that(paste(e, "weighted oomglm"), {
  #   eqn <- make_formula(e, df)
  #   y <- glm(eqn, data = df, weights = w)
  #   x <- iter_model(df, eqn, weights = ~w)
  #   expect_attr_equal(x, y, df)
  # })
  # 
}



# 
# 
# test_that("updating oomglm without intercept", {
# 
#   for(e in colnames(df)[1:5]) {
#     f <- make_formula(e, df, noint = TRUE)
#     y <- glm(f, data = df)
#     x <- iter_model(df, f)
#     expect_attr_equal(x, y, df)
#   }
#   
# })
# 
# 
# test_that("weighted updating oomglm without intercept", {
# 
#   for(e in colnames(df)[1:5]) {
#     f <- make_formula(e, df, noint = TRUE)
#     y <- glm(f, data = df, weights = w)
#     x <- iter_model(df, f, weights = ~w)
#     expect_attr_equal(x, y, df)
#   }
#   
# })


# test_that("weighted oomglm with zero weight", {
# 
#   dfx <- df
#   dfx[4:7, 'w']  <- 0.0
# 
#   for(e in colnames(dfx)[1:5]) {
#     f <- make_formula(e, dfx)
#     y <- glm(f, data = dfx, weights = w)
#     x <- iter_model(dfx, f, weights = ~w)
#     expect_attr_equal(x, y, dfx)
#   }
# 
# })
