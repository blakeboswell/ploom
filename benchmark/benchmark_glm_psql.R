
feed_w_reset <- function(con, query, chunksize) {
  
  result <- RPostgres::dbSendQuery(con, query)
  got    <- 0
  
  function(reset = FALSE){
    
    if(reset) {
      if(got > 0) {
        
        if(DBI::dbIsValid(result)) {
          RPostgres::dbClearResult(result)
        }
        result <<- RPostgres::dbSendQuery(con, query)
        got    <<- 0
      }
      return(TRUE)
    }
    
    rval <- DBI::dbFetch(result, n = chunksize)
    got  <<- got + nrow(rval)
    
    if (nrow(rval) == 0) {
      return(NULL)
    }
    return(rval)
    
  }
  
}


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
    "oomglm" = {

      con   <- psql_con()
      rs    <- RPostgres::dbSendQuery(con, query)
      feed  <- oomdata_dbi(rs, chunk_size = chunk_size)

      x     <- iter_weight(
        oomglm(formula = lm_formula),
        data = feed,
        max_iter = 8L
      )

      # RPostgres::dbDisconnect(con)
      
      coef(x)

    },
    "bigglm" = {

      con   <- psql_con()
      feed  <- feed_w_reset(con, query, chunksize = chunk_size)
      y     <- bigglm(formula = lm_formula, data = feed)

      # RPostgres::dbDisconnect(con)
      
      coef(y)

    },
    "speedglm" = {

      con   <- psql_con()
      feed  <- feed_w_reset(con, query, chunksize = chunk_size)
      z     <- shglm(formula = lm_formula, datafun = feed)

      # RPostgres::dbDisconnect(con)
      
      coef(z)

    },
    min_time   = Inf,
    iterations = 1,
    check      = FALSE
  ) %>%
    mutate(
      num_obs    = num_obs,
      chunk_size = chunk_size
    ) %>%
    select(-gc)

  bm
  
}


main <- function(table_prefix, num_obs) {
  
  num_div <- 5
  divs    <- 1:num_div*(num_obs/num_div)
  chunk_sizes <- divs[[1]] / 2
  
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
