
library(biglm)
library(speedglm)

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
query   <- glue("
  select {vars_nm}
  from {TABLE_PREFIX}_data;
  ")


vars_fun <- str_c(
  head(vars, 1),
  "~",
  str_c(tail(vars, -1), collapse = "+")
) %>%
  as.formula()


bm <- microbenchmark(
  "oomlm" = {
    
    message("oomlm start")
    
    rs    <- RPostgres::dbSendQuery(con, query)
    feed  <- oom_data(rs, 10^6)
    
    x     <- oomlm(formula <- vars_fun)
    
    while(!is.null(chunk <- feed())) {
      x <- update(x, data = chunk)
    }
    
    message("oomlm end")
    
  },
  "biglm" = {
    
    message("biglm start")
    
    rs    <- RPostgres::dbSendQuery(con, query)
    feed  <- oom_data(rs, 10^6)
    
    count <- 0
    
    while(!is.null(chunk <- feed())) {
      count <- count + 1
      if(count == 1) {
        y <- biglm(formula = vars_fun, data = chunk)
      } else {
        y <- update(y, chunk)
      }
    }
    
    message("biglm end")
    
  },
  "speedlm" = {
    
    message("speedlm start")
    
    rs    <- RPostgres::dbSendQuery(con, query)
    feed  <- oom_data(rs, 10^6)
    
    count <- 0
    
    while(!is.null(chunk <- feed())) {
      count <- count + 1
      if(count == 1) {
        z <- speedlm(formula = vars_fun, data = chunk)
      } else {
        z <- update(z, data = chunk)
      }
    }
    
    message("speedlm end")
    
  },
  times = 5
)

bm

check  <-
  RPostgres::dbSendQuery(
    con,
    glue("select * from {TABLE_PREFIX}_covariate;")
  ) %>%
  RPostgres::dbFetch() %>%
  as.numeric() %>%
  set_names(names(coef(x)))

all.equal(coef(x), coef(y), coef(z), check)


RPostgres::dbClearResult(rs)
RPostgres::dbDisconnect(con)
