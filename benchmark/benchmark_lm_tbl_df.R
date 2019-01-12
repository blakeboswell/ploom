
select_data <- function(table_prefix, num_obs) {

  con <- psql_con()
  
  vars <-
    RPostgres::dbSendQuery(con, glue("select * from {table_prefix}_data")) %>%
    RPostgres::dbColumnInfo() %>%
    pull(name) %>%
    discard(function(x) x %in% c("index", "real_alpha"))
  
  query   <- glue(
    "select {str_c(vars, collapse = '\n, ')}
    from {table_prefix}_data
    limit {num_obs};
    ")
  
  RPostgres::dbSendQuery(con, query) %>%
    RPostgres::dbFetch()  %>%
    as_tibble()
  
}

make_formula <- function(var_names) {
  y <- head(var_names, 1)
  X <- tail(var_names, -1)
  str_c(y, "~", str_c(X, collapse = "+")) %>% as.formula()
}


benchmark_lm <- function(num_obs, df) {
  
  message(glue("benchmark num_obs (M): {num_obs/10^6}"))
  
  bench::mark(
    "lm" = {
      u <- lm(formula = lm_formula, data = df[1:num_obs, ])
    },
    "oomlm" = {
      x  <- update(oomlm(formula = lm_formula), data = df[1:num_obs, ])
    },
    "biglm" = {
      y <- biglm(formula = lm_formula, data = df[1:num_obs, ])
    },
    "speedlm" = {
      z <- speedlm(formula = lm_formula, data = df[1:num_obs, ])
    },
    min_time   = Inf,
    iterations = 5,
    check      = FALSE
  ) %>%
    summary()   %>%
    mutate(num_obs = num_obs) %>%
    select(-memory, -gc)
  
}


main <- function(table_prefix, num_obs) {
  
  df         <- select_data(table_prefix, num_obs)
  lm_formula <- df %>% colnames() %>% make_formula()
  
  # speedlm requires that the formula be in the global environment
  assign("lm_formula", lm_formula, envir = globalenv())
  
  res  <- map_df(1:5*(1/2)*(num_obs/5), benchmark_lm, df = df)

  rm(lm_formula, pos = 1)
  
  res

}







