
# packages --------------------------------------------------------------

library(microbenchmark)
library(bench)

library(RPostgres)

library(furrr)
library(purrr)
library(stringr)
library(glue)
library(dplyr)

library(biglm)
library(speedglm)


## connections ----------------------------------------------------------

psql_con <- function() {
  
  RPostgres::dbConnect(
    drv      =  RPostgres::Postgres(),
    dbname   = "ploom_benchmark",
    host     = "localhost",
    port     = 5432,
    user     = Sys.getenv("USER"),
    password = "qwerty"
  )
  
}


## create data  ----------------------------------------------------------

data_gen <- new.env()

sys.source(
  file  = file.path(getwd(), "benchmark", "benchmark_data.R"),
  envir        = data_gen,
  toplevel.env = data_gen
)

data_gen$main(
  N            = 10^6,
  p            = 50,
  chunk_size   = 10^5,
  table_prefix = "linear", 
  nprocs       = 4,
  seed         = 2001  
)


# in-memory benchmark ---------------------------------------------------

tbl_df_bm <- new.env()

sys.source(
  file  = file.path(getwd(), "benchmark", "benchmark_tbl_df.R"),
  envir        = tbl_df_bm,
  toplevel.env = tbl_df_bm
)

result_tbl <- tbl_df_bm$main(table_prefix = "linear", num_obs = 10^6)


# psql feed benchmark ---------------------------------------------------

psql_bm <- new.env()

sys.source(
  file  = file.path(getwd(), "benchmark", "benchmark_psql.R"),
  envir        = psql_bm,
  toplevel.env = psql_bm
)

result_psql <- psql_bm$main(table_prefix = "linear", num_obs = 10^6)
