
select_data <- function(table_prefix, num_obs) {

  con <- psql_con()
  
  vars <-
    RPostgres::dbSendQuery(con, glue("select * from {table_prefix}_data")) %>%
    RPostgres::dbColumnInfo() %>%
    pull(name) %>%
    discard(function(x) x %in% c("index")) %>%
    discard(function(x) str_detect(x, "y") & x != "real_ygauss")
  
  query   <- glue(
    "select {str_c(vars, collapse = '\n, ')}
    from {table_prefix}_data
    limit {num_obs};
    ")
  
  RPostgres::dbSendQuery(con, query) %>%
    RPostgres::dbFetch()  %>%
    as_tibble()
  
}

make_formula <- function(var_names, yval) {
  y <- yval
  X <- var_names %>% 
    discard(function(x) str_detect(x, "y"))
  str_c(y, "~", str_c(X, collapse = "+")) %>% as.formula()
}


benchmark_lm <- function(num_obs, df) {
  
  message(glue("benchmark num_obs (M): {num_obs/10^6}"))
  
  bm <- bench::mark(
    "lm" = {
      u <- lm(formula = lm_formula, data = df[1:num_obs, ])
      coef(u)
    },
    "oomlm" = {
      x  <- fit(oomlm(formula = lm_formula), data = df[1:num_obs, ])
      coef(x)
    },
    "oomlm + resid" = {
      x <- fit(oomlm(formula = lm_formula), data = df[1:num_obs, ])
      y <- predict(x, new_data = df[1:num_obs, ])
      u <- df[1:num_obs, 1] - y
      coef(x)
    },
    "biglm" = {
      y <- biglm(formula = lm_formula, data = df[1:num_obs, ])
      coef(y)
    },
    "biglm + resid" = {
      z <- biglm(formula = lm_formula, data = df[1:num_obs, ])
      y <- predict(z,  df[1:num_obs, ])
      u <- df[1:num_obs, 1] - y
      coef(z)
    },
    "speedlm" = {
      z <- speedlm(formula = lm_formula, data = df[1:num_obs, ])
      coef(z)
    },
    "speedlm + resid" = {
      z <- speedlm(formula = lm_formula, data = df[1:num_obs, ])
      y <- predict(z,  df[1:num_obs, ])
      u <- df[1:num_obs, 1] - y
      coef(z)
    },
    min_time   = Inf,
    iterations = 5,
    check      = FALSE
  ) %>%
    summary()   %>%
    mutate(num_obs = num_obs)

  bm
  
}


# table_prefix <- "linear"
# num_obs      <- 5000
# 
# df <- select_data(table_prefix, num_obs)
# 
# lm_formula <- df %>% 
#   colnames() %>% 
#   make_formula(yval = "real_ygauss")


main <- function(table_prefix, num_obs) {
  
  num_div <- 5
  divs    <- 1:num_div*(num_obs/num_div)
  
  df         <- select_data(table_prefix, num_obs)
  lm_formula <- df %>% colnames() %>% make_formula(yval = "real_ygauss")
  
  # speedlm requires that the formula be in the global environment
  assign("lm_formula", lm_formula, envir = globalenv())
  
  res  <- map_df(divs, benchmark_lm, df = df)

  rm(lm_formula, pos = 1)
  
  res

}
