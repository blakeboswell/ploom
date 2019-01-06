
library(biglm)
library(speedglm)
library(estimatr)


library(glue)
library(purrr)
library(microbenchmark)
library(stringr)
library(RPostgres)
library(dplyr)

TABLE_PREFIX <- "test"

con <- RPostgres::dbConnect(
  drv      =  RPostgres::Postgres(),
  dbname   = "ploom_benchmark",
  host     = "localhost",
  port     = 5432,
  user     = Sys.getenv("USER"),
  password = rstudioapi::askForPassword("Database password")
)

vars <-
  RPostgres::dbSendQuery(con, glue("select * from {TABLE_PREFIX}_data")) %>%
  RPostgres::dbColumnInfo() %>%
  pull(name) %>%
  discard(function(x) x == "index")


vars_nm <- str_c(vars, collapse = "\n, ")
query   <- glue(
  "select {vars_nm}
   from {TABLE_PREFIX}_data
   limit 10^7;
")


vars_fun <- str_c(
  head(vars, 1),
  "~",
  str_c(tail(vars, -1), collapse = "+")
) %>%
  as.formula()


df <- RPostgres::dbSendQuery(con, query) %>%
  RPostgres::dbFetch()  %>%
  as_tibble()


benchmark_lm <- function(n) {
  
  message(glue("benchmark {n/10^6}"))
  
  microbenchmark(
    "lm" = {
      u <- lm(formula = vars_fun, data = df[1:n, ])
    },
    "lm_robust" = {
      v <- lm_robust(
        formula = vars_fun, data = df[1:n, ],
        se_type = "classical"
      )
    },
    "oomlm" = {
      
      feed  <- oom_data(df[1:n, ], 5 * 10^5)
      x     <- oomlm(formula = vars_fun)
      
      while(!is.null(chunk <- feed())) {
        x <- update(x, data = chunk)
      }
      
    },
    "biglm" = {

      feed  <- oom_data(df[1:n, ], 5 * 10^5)
      count <- 0

      while(!is.null(chunk <- feed())) {
        count <- count + 1
        if(count == 1) {
          y <- biglm(formula = vars_fun, data = chunk)
        } else {
          y <- update(y, chunk)
        }
      }

    },
    "speedlm" = {
      
      feed  <- oom_data(df[1:n, ], 5 * 10^5)
      count <- 0

      while(!is.null(chunk <- feed())) {

        count <- count + 1
        if(count == 1) {
          z <- speedlm(formula = vars_fun, data = chunk)
        } else {
          z <- update(z, data = chunk)
        }
      }

    },
    times = 5
  ) %>%
    summary() %>%
    as_tibble() %>%
    mutate(num_obs = n)
  
}


tbl_bm <- map_df(1:10*10^6, benchmark_lm)

tbl_bm %>%
  saveRDS(file = "benchmark/tbl_bm.Rds")


library(ggplot2)

tbl_bm %>%
  mutate(num_obs = num_obs) %>%
  ggplot() +
  geom_line(aes(y = mean, x = num_obs, col = expr)) +
  theme_bw() +
  theme(
    plot.margin = margin(t = 10, r = 15, b = 10, l = 15) 
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = 1:10*10^6) +
  labs(
    title    = "Linear Model Implementations on in-memory Data",
    y        = "Mean Seconds",
    x        = "Number of Observations",
    caption  = "Mean seconds "
  )








