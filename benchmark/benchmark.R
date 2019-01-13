
# packages --------------------------------------------------------------

library(biglm)
library(speedglm)

library(RPostgres)

library(bench)

library(furrr)
library(purrr)
library(stringr)
library(glue)
library(broom)
library(dplyr)


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
  N            = 10^7,
  p            = 50,
  chunk_size   = 10^5,
  table_prefix = "linear",
  nprocs       = 4,
  seed         = 2001
)


## in-memory lm benchmark -----------------------------------------------

lm_tbl_df_env <- new.env()

sys.source(
  file  = file.path(getwd(), "benchmark", "benchmark_lm_tbl_df.R"),
  envir        = lm_tbl_df_env,
  toplevel.env = lm_tbl_df_env
)

result_tbl <- lm_tbl_df_env$main(table_prefix = "linear", num_obs = 10^6)

result_lm_tbl %>%
  saveRDS("benchmark/results/lm_tbl_df.Rds")


## in-memory glm benchmark ----------------------------------------------

glm_tbl_df_env <- new.env()

sys.source(
  file  = file.path(getwd(), "benchmark", "benchmark_glm_tbl_df.R"),
  envir        = glm_tbl_df_env,
  toplevel.env = glm_tbl_df_env
)

result_glm_tbl <- glm_tbl_df_env$main(table_prefix = "linear", num_obs = 10^6)

result_glm_tbl %>%
  saveRDS("benchmark/results/glm_tbl_df.Rds")


# psql lm benchmark -----------------------------------------------------

lm_psql_env <- new.env()

sys.source(
  file  = file.path(getwd(), "benchmark", "benchmark_lm_psql.R"),
  envir        = lm_psql_env,
  toplevel.env = lm_psql_env
)

result_lm_psql <- lm_psql_env$main(table_prefix = "linear", num_obs = 10^7)

result_lm_psql %>%
  saveRDS("benchmark/results/lm_psql.Rds")


# psql glm benchmark ----------------------------------------------------

glm_psql_env <- new.env()

sys.source(
  file  = file.path(getwd(), "benchmark", "benchmark_glm_psql.R"),
  envir        = glm_psql_env,
  toplevel.env = glm_psql_env
)

result_glm_psql <- glm_psql_env$main(table_prefix = "linear", num_obs = 10^6)

result_glm_psql %>%
  saveRDS("benchmark/results/glm_psql.Rds")

