# 
# # data for `oomdata_con()` tests ----------------------------------------
# #
# 
# write.table(x = mtcars, row.names = FALSE, file = "./tests/testdata/mtcars.txt")
# system("gzip ./tests/testdata/mtcars.txt")
# write.table(x = mtcars, row.names = FALSE, file = "./tests/testdata/mtcars.txt")
# 
# 
# # data for `oomlm_robust()` tests ----------------------------------------
# #
# 
# library(estimatr)
# library(purrr)
# library(dplyr)
# 
# 
# build_lm_robust_models <- function(se_type, df) {
# 
#   eqn1 <- mpg ~ cyl + disp + hp + wt
#   eqn2 <- mpg ~ 0 + cyl + disp + hp + wt
# 
#   x1  <- lm_robust(eqn1, data = df, se_type = se_type)
#   x2  <- lm_robust(eqn2, data = df, se_type = se_type)
#   x3  <- lm_robust(eqn1, weights = w, data = df, se_type = se_type)
#   x4  <- lm_robust(eqn2, weights = w, data = df, se_type = se_type)
# 
#   list(x1, x2, x3, x4)
# 
# }
# 
# build_lm_robust_outputs <- function(se_type, df) {
# 
#   models <- build_lm_robust_models(se_type, df)
# 
#   fn <- function(x) {
#     px1 <- predict(x, df)
#     px2 <- predict(x, df, se.fit = TRUE)
#     px3 <- predict(x, df, se.fit = TRUE, interval = "confidence")
#     px4 <- predict(x, df, se.fit = TRUE, interval = "prediction")
# 
#     list(
#       tidy_x  = tidy(x),
#       none    = px1,
#       se_fit  = px2,
#       conf    = px3,
#       pred    = px4
#     )
#   }
# 
#   map(models, fn)
# 
# }
# 
# set.seed(42)
# 
# se_types <- c("HC0", "HC1", "stata", "classical")
# df       <- mtcars
# w        <- runif(nrow(mtcars))
# df['w']  <- w / sum(w)
# 
# models_by_type <- map(se_types, build_lm_robust_outputs, df = df) %>%
#   set_names(se_types)
# 
# saveRDS(models_by_type, "./tests/testdata/lm_robust.Rds")
