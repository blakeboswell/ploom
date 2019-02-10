
#' create comma delimited "name type" list for sql statements
#' based on column names (expected to be of the form "name_type")
#'
#' @param col_names
#' @keywords internal
psql_specs <- function(col_names, type_override = NULL) {
  
  if(!is.null(type_override)) {
    types <- str_c(" ", type_override)
  } else {
    
    types <- str_sub(
      col_names,
      start = 1,
      end   = str_locate(col_names, "_")[, 1] - 1
    )
    
    types <- str_c(" ", types)
  
  }
  
  col_names %>%
    str_c(., types, collapse="\n, ")
}


#' create ddl for table to hold a, B data
#' 
#' @param col_names
#' @param table_name
#' @keywords internal
covariate_ddl <- function(col_names, table_name) {
  
  col_specs <- psql_specs(col_names, "real")
  
  glue("create table {table_name} (
      {col_specs}
    );"
       
  )
}


#' create and load covariate table
#' 
#' @param con
#' @param seed
#' @param p
#' @param table_prefix
#' @keywords internal
create_load_covariate_psql <- function(con, seed, params, table_prefix) {
  
  set.seed(seed)
  
  params_df  <- params_as_tibble(params)
  table_name <- str_c(table_prefix, "_covariate")
  
  RPostgres::dbExecute(
    con,
    params$names %>%
      covariate_ddl(., table_name)
  )
  
  RPostgres::dbWriteTable(
    con,
    table_name,
    params_df,
    append = TRUE, row.names = FALSE
  )
  
}


#' create ddl for table to hold X, y data
#'
#' @param col_names
#' @param table_name
#' @keywords internal
data_ddl <- function(col_names, table_name) {
  
  col_names <- col_names[col_names != "real_intercept"]
  
  glue(
    "create table {table_name} (
    index serial primary key
    , {psql_specs(col_names)}
    );"
  )
  
}


#' create data table
#'
#' @param con
#' @param params
#' @param table_prefix
#' 
#' @keywords internal
create_data_psql <- function(con, params, table_prefix) {
  
  resp_names <- paste(
    "real", c("y", "ygauss", "ybin", "ypois", "ygamma"),  sep = "_")
  
  col_names  <- c(resp_names, params$names)
  table_name <- str_c(table_prefix, "_data")
  
  RPostgres::dbExecute(
    con = psql_con(),
    col_names %>%
      data_ddl(., table_name)
  )
  
}


#' generate data and linear response and load to psql
#' 
#' @param seed seed for the rng used in this load process
#' @param N number of rows to insert
#' @param chunk_size number of rows to insert per loop
#' @param table_name name of table to insert rows
#'
#' @keywords internal
load_data_psql <- function(seed, N, params, chunk_size, table_prefix) {
  
  con <- psql_con()
  
  set.seed(seed)
  count <- 0
  
  while(count < N) {
    
    df <- generate_data(params$betas, chunk_size)
    
    RPostgres::dbWriteTable(
      con,
      str_c(table_prefix, "_data"), 
      df,
      append = TRUE, row.names = FALSE
    )
    count <- count + chunk_size
  }
  
  count
}


# load data to psql --------------------------------------------
#

main <- function(N            = 10^6,
                 p            = 50,
                 chunk_size   = 10^5,
                 table_prefix = "linear", 
                 nprocs       = 4,
                 seed         = 2001) {

  set.seed(seed)
  covr_seed <- runif(1, 0L, .Machine$integer.max)
  data_seed <- runif(nprocs, 0L, .Machine$integer.max)
  
  
  # create and load covariate table ---------------------------------------
  #
  
  params <- linear_params(p)
  
  create_load_covariate_psql(
    con    = psql_con(),
    seed   = covr_seed,
    params = params,
    table_prefix = table_prefix
  )
  
  # create data table ------------------------------------------------------
  
  create_data_psql(
    con    = psql_con(),
    params = params,
    table_prefix = table_prefix
  )
  
  # load data table -------------------------------------------------------
  
  plan(multiprocess, workers = nprocs)
  
  time_bm <- Sys.time()
  
  temp <- furrr::future_map_dbl(
    data_seed,
    function(seed) {
      load_data_psql(
        seed,
        N = N / nprocs,
        params       = params,
        chunk_size   = chunk_size,
        table_prefix = table_prefix
      )
    },
    .progress = TRUE
  )
  
  run_time <- Sys.time() - time_bm
  
  message(run_time)
  
}

