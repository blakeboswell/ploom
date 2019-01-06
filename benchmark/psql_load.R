
library(dplyr)
library(purrr)
library(stringr)
library(glue)
library(RPostgres)

source(file.path(getwd(), "benchmark", "generate_data.R"))

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
  
  col_names <- c("real_alpha", col_names %>% tail(-1))
  col_specs <- psql_specs(col_names, "real")
  
  glue("create table {table_name} (
      {col_specs}
    );"
       
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


#' generate data and linear response and load to psql
#' 
#' @param con Rpostgres connection
#' @param N number of rows to insert
#' @param p number of column in X
#' @param chunk_size number of rows to insert per loop
#' @param table_name name of table to insert rows
#'
#' @keywords internal
psql_load <- function(con, N, p, chunk_size, table_prefix, seed = 42) {
  
  set.seed(seed)
  
  params <- linear_params(p)
  count  <- 0
  
  data_name      <- str_c(table_prefix, "_data")
  covariate_name <- str_c(table_prefix, "_covariate")
  
  while(count < N) {
    
    df <- generate_data(params$alpha, params$betas, chunk_size)
    
    if(count < 1) {
      
      RPostgres::dbExecute(
        con,
        colnames(df) %>%
          covariate_ddl(., covariate_name)
      )
      
      params_df <- params_as_tibble(params, colnames(df))
      
      RPostgres::dbWriteTable(
        con, covariate_name, params_df, append = TRUE, row.names = FALSE
      )
      
      RPostgres::dbExecute(
        con,
        colnames(df) %>%
          data_ddl(., data_name)
      )

    }
    
    RPostgres::dbWriteTable(con, data_name, df, append = TRUE, row.names = FALSE)
    
    count <- count + chunk_size
    message(count / chunk_size)
    
  }
  
}


# load data to psql -----------------------------------------------------

con <- RPostgres::dbConnect(
  drv      =  RPostgres::Postgres(),
  dbname   = "ploom_benchmark",
  host     = "localhost",
  port     = 5432,
  user     = "blakeboswell",
  password = rstudioapi::askForPassword("Database password")
)

psql_load(
  con,
  N          = 10^7,
  p          = 20,
  chunk_size = 10^6,
  table_prefix = "test",
  seed         = 2001
)

