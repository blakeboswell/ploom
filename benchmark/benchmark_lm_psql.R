
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
    order by index
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
  
  bm <- bench::mark(
    "oomlm" = {

      con   <- psql_con()
      rs    <- RPostgres::dbSendQuery(con, query)
      feed  <- oom_data(rs, chunk_size = chunk_size)
      x     <- oomlm(formula = lm_formula)

      while(!is.null(chunk <- feed())) {
        x <- update(x, data = chunk)
      }

      RPostgres::dbDisconnect(con)
      
      coef(x)

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
      
      coef(y)

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
      
      coef(z)

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
  
  bm
  
}


main <- function(table_prefix, num_obs) {
  
  num_div <- 5
  divs    <- 1:num_div*(num_obs/num_div)
  chunk_sizes <- divs / 20
  
  vars       <- model_vars(psql_con(), table_prefix)
  lm_formula <- vars %>% make_formula()
  
  # speedlm requires that the formula be in the global environment
  assign("lm_formula", lm_formula, envir = globalenv())
  
  res <- map2_df(
    .x = divs,
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
