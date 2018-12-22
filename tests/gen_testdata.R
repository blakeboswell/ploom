library(estimatr)

gen_models <- function(fxn, data) {
  
  se_types <- c("classical", "HC0", "HC1", "stata")
  mdls     <- lapply(
    se_types, 
    function(se_type) {
      summary(lm_robust(fxn, data = data, se_type = se_type, weights = w))
    })
  
  names(mdls) <- se_types
  
  mdls
  
}


df      <- mtcars
df['w'] <- 1

mdls       <- gen_models(mpg ~ cyl + disp + hp + wt, data = df)
mdls_noint <- gen_models(mpg ~ 0 + cyl + disp + hp + wt, data = df)

set.seed(42)
w       <- runif(nrow(mtcars))
df['w'] <- w / sum(w)

mdls_wt       <- gen_models(mpg ~ cyl + disp + hp + wt, data = df)
mdls_noint_wt <- gen_models(mpg ~ 0 + cyl + disp + hp + wt, data = df)

saveRDS(list(
  regular = mdls,
  noint   = mdls_noint,
  weights = mdls_wt,
  weights_noint = mdls_noint_wt
), file = "tests/testdata/robust-models.Rds")

