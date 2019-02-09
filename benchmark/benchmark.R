
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

# benchmark_env <- new.env()
# 
# sys.source(
#   file  = file.path(getwd(), "benchmark", "benchmark_data.R"),
#   envir        = benchmark_env,
#   toplevel.env = benchmark_env
# )
# 
# benchmark_env$main(
#   N            = 10^7,
#   p            = 20,
#   chunk_size   = 10^5,
#   table_prefix = "linear",
#   nprocs       = 4,
#   seed         = 2001
# )


## in-memory lm benchmark -----------------------------------------------

benchmark_env <- new.env()

sys.source(
  file  = file.path(getwd(), "benchmark", "benchmark_lm_tbl_df.R"),
  envir        = benchmark_env,
  toplevel.env = benchmark_env
)

result <- benchmark_env$main(table_prefix = "linear", num_obs = 5 * 10^6)

result %>%
  saveRDS("benchmark/results/lm_tbl_df.Rds")



## in-memory glm benchmark ----------------------------------------------

benchmark_env <- new.env()

sys.source(
  file  = file.path(getwd(), "benchmark", "benchmark_glm_tbl_df.R"),
  envir        = benchmark_env,
  toplevel.env = benchmark_env
)

result <- benchmark_env$main(table_prefix = "linear", num_obs = 10^6)

result %>%
  saveRDS("benchmark/results/glm_tbl_df.Rds")



# psql lm benchmark -----------------------------------------------------

benchmark_env <- new.env()

sys.source(
  file  = file.path(getwd(), "benchmark", "benchmark_lm_psql.R"),
  envir        = benchmark_env,
  toplevel.env = benchmark_env
)

result <- benchmark_env$main(table_prefix = "linear", num_obs = 10^6)

result %>%
  saveRDS("benchmark/results/lm_psql.Rds")


# psql glm benchmark ----------------------------------------------------

benchmark_env <- new.env()

sys.source(
  file  = file.path(getwd(), "benchmark", "benchmark_glm_psql.R"),
  envir        = benchmark_env,
  toplevel.env = benchmark_env
)

result <- benchmark_env$main(table_prefix = "linear", num_obs = 10^6)

result %>%
  saveRDS("benchmark/results/glm_psql.Rds")

