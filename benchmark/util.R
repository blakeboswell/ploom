
rlang::env(

  con = function() {
    RPostgres::dbConnect(
      drv      =  RPostgres::Postgres(),
      dbname   = "ploom_benchmark",
      host     = "localhost",
      port     = 5432,
      user     = Sys.getenv("USER"),
      password = "qwerty"
    )
  }
    
)