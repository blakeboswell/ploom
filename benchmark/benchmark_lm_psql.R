
model_vars <- function(con, table_prefix) {
  RPostgres::dbSendQuery(con, glue("select * from {table_prefix}_data")) %>%
    RPostgres::dbColumnInfo() %>%
    pull(name) %>%
    discard(function(x) x %in% c("index", "real_alpha"))
}

feed_query <- function(table_prefix, vars, num_obs) {
  glue(
    "select {str_c(vars, collapse = '\n, ')}
    from {table_prefix}_data
    limit {num_obs};
    ")
}

make_formula <- function(vars) {
  y <- head(vars, 1)
  X <- tail(vars, -1)
  str_c(y, "~", str_c(X, collapse = "+")) %>% as.formula()
}


benchmark_lm <- function(num_obs, chunk_size, table_prefix, vars) {
  
  message(glue("benchmark num_obs (M): {num_obs/10^6}"))
  query      <- feed_query(table_prefix, vars, num_obs)
  
  bench::mark(
    "oomlm" = {

      con   <- psql_con()
      rs    <- RPostgres::dbSendQuery(con, query)
      feed  <- oom_data(rs, chunk_size = chunk_size)
      x     <- oomlm(formula = lm_formula)

      while(!is.null(chunk <- feed())) {
        x <- update(x, data = chunk)
      }

      RPostgres::dbDisconnect(con)

    },
    "biglm" = {

      con   <- psql_con()
      rs    <- RPostgres::dbSendQuery(con, query)
      feed  <- oom_data(rs, chunk_size = chunk_size)

      count <- 0

      while(!is.null(chunk <- feed())) {
        count <- count + 1
        if(count == 1) {
          y <- biglm(formula = lm_formula, data = chunk)
        } else {
          y <- update(y, chunk)
        }
      }

      RPostgres::dbDisconnect(con)

    },
    "speedlm" = {

      con   <- psql_con()
      rs    <- RPostgres::dbSendQuery(con, query)
      feed  <- oom_data(rs, chunk_size = chunk_size)

      count <- 0

      while(!is.null(chunk <- feed())) {
        count <- count + 1
        if(count == 1) {
          z <- speedlm(formula = lm_formula, data = chunk)
        } else {
          z <- update(z, data = chunk)
        }
      }

      RPostgres::dbDisconnect(con)

    },
    min_time   = Inf,
    iterations = 5,
    check      = FALSE
  ) %>%
    mutate(
      num_obs    = num_obs,
      chunk_size = chunk_size
    ) %>%
    select(-memory, -gc)
  
}


main <- function(table_prefix, num_obs) {
  
  num_obs_vec <- 1:5*(1/2)*(num_obs/5)
  chunk_sizes <- num_obs_vec / 5
  
  vars       <- model_vars(psql_con(), table_prefix)
  lm_formula <- vars %>% make_formula()
  
  # speedlm requires that the formula be in the global environment
  assign("lm_formula", lm_formula, envir = globalenv())
  
  res <- map2_df(
    .x = num_obs_vec,
    .y = chunk_sizes,
    benchmark_lm,
    table_prefix = table_prefix,
    vars         = vars
  )

  rm(lm_formula, pos = 1)

  res

}


# check  <-
#   RPostgres::dbSendQuery(
#     con,
#     glue("select * from {TABLE_PREFIX}_covariate;")
#   ) %>%
#   RPostgres::dbFetch() %>%
#   as.numeric() %>%
#   set_names(names(coef(x)))
# 
# all.equal(coef(x), coef(y), coef(z), check)
# 
# 
# RPostgres::dbClearResult(rs)
# RPostgres::dbDisconnect(con)
