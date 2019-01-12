


#' make a and B for y = a + BX + e
#'
#' @param p number of columns in X.  will be forced to the nearest
#'   number divisible by two
#' @param alpha_lim alpha will be randomly picked in
#'   range between `-alpha_lim` and `alpha_lim`
#' @param betas_mu_lim mean of beta normal distributions
#'   range between `-betas_mu_lim` and `betas_mu_lim`
#' @param betas_sd_lim standard deviation of beta normal distributions
#'   range between 1 and `betas_sd_lim`
linear_params <- function(p, 
                          alpha_lim     = 10,
                          betas_mu_lim  = 10,
                          betas_sd_lim  = 4) {
  
  p <- 2 * ceiling(p / 2)
  
  betas_mu  <- runif(p, -betas_mu_lim, betas_mu_lim)
  betas_sd  <- runif(p, 1, betas_sd_lim)
  
  X <- map2_dbl(betas_mu, betas_sd, function(x, y) rnorm(1, x, y))
  
  col_names <- c(
    "real_alpha",
    str_c("real", 1:(p/2), sep = "_"),
    str_c("int", 1:(p/2), sep = "_")
  )
  
  list(
    alpha = runif(1, -alpha_lim, alpha_lim),
    betas = matrix(X, ncol = 1),
    names = col_names
  )
}


#' transform param list into single row table
#' with alpha and B having names corresponding to data
#'
#' @param params
#' @param col_names
#' @keywords internal
params_as_tibble <- function(params) {
  
  matrix(c(params$alpha, params$betas), nrow = 1) %>%
    as_tibble() %>%
    set_names(params$names)
  
} 


#' create data with linear relationship between y and x
#'
#' @param alpha
#' @param betas
#' @param nrows
#' @param sigma
#' 
#' @keywords internal
generate_data <- function(alpha, betas, nrows, sigma = 10) {
  
  p <- length(betas) / 2
  
  X1 <- matrix(
    runif(nrows * p, min = 0, max = 100),
    nrow = nrows,
    ncol = p
  ) 
  
  colnames(X1) <- str_c("real", 1:p, sep = "_")
  
  X2 <- mapply(
    function(num_cat, n) {
      sample(1:num_cat, n, replace = TRUE)
    },
    num_cat = 2:(p+1),
    n = nrows
  )
  colnames(X2) <- str_c("int", 1:p, sep = "_")
  
  # eventually add categorical values
  #
  # X3 <- mapply(
  #   function(num_cat, n) {
  #     as.factor(sample(letters[1:num_cat], n, replace = TRUE))
  #   },
  #   num_cat = 2:(p+1),
  #   n = nrows
  # )
  # colnames(X3) <- str_c("text", 1:p, sep = "_")
  
  X <- cbind(X1, X2)
  Y <- alpha + X %*% betas + rnorm(nrows, 0, sigma)
  
  cbind(Y, X) %>%
    as_tibble() %>%
    rename_all(tolower) %>%
    rename(real_y = v1)
  
}



# -----------------------------------------------------------------------



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
    params_as_tibble(params),
    append = TRUE, row.names = FALSE
  )
  
}


#' create ddl for table to hold X, y data
#'
#' @param col_names
#' @param table_name
#' @keywords internal
data_ddl <- function(col_names, table_name) {
  
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
#' @param p
#' @param table_prefix
#' @keywords internal
create_data_psql <- function(con, params, table_prefix) {
  
  col_names <- c("real_y", params$names)
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
#' @param p number of column in X
#' @param chunk_size number of rows to insert per loop
#' @param table_name name of table to insert rows
#'
#' @keywords internal
load_data_psql <- function(seed, N, p, params, chunk_size, table_prefix) {
  
  con <- psql_con()
  
  set.seed(seed)
  count <- 0
  
  while(count < N) {
    
    df <- generate_data(params$alpha, params$betas, chunk_size)
    
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
        p = p,
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

