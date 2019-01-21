context("test-oomdata.R")

test_connection <- function(con, data_frame) {

  cursor     <- 0  
  chunk_size <- 5
  n <- nrow(data_frame)
  
  if(inherits(con, "DBIResult")) {
    tmp <- oomdata_dbi(con, chunk_size = chunk_size)  
  } else {
    tmp <- oomdata_con(con, chunk_size = chunk_size, header = TRUE)  
  }
  
  for (i in 1:3) {
    cursor <- 0
    while(!is.null(x <- tmp())) {
      start  <- cursor + 1
      cursor <- cursor + min(chunk_size, n - cursor)
      rownames(x) <- start:cursor
      expect_equal(x, data_frame[start:cursor, ])
    }
  }
  
}


test_that("data.frame", {

  chunk_size <- 5
  df     <- mtcars
  n      <- nrow(df)
  tmp    <- oomdata_tbl(df, chunk_size = chunk_size)

  for (i in 1:3) {
    cursor <- 0
    while(!is.null(x <- tmp())) {
      start  <- cursor + 1
      cursor <- cursor + min(chunk_size, n - cursor)
      expect_equal(x, df[start:cursor, ])
    }
  }
})


test_that("file", {
  skip_on_travis()
  df  <- mtcars
  rownames(df) <- NULL
  test_connection(file("../testdata/mtcars.txt"), df)
})


test_that("gzfile", {
  skip_on_travis()
  df <- mtcars
  rownames(df) <- NULL
  test_connection(gzfile("../testdata/mtcars.txt.gz"), df)
})


test_that("dbi_connection", {
  skip_on_travis()
  con <- DBI::dbConnect(RSQLite::SQLite(), path = ":dbname:")
  
  dplyr::copy_to(con, mtcars, "mtcars", temporary = FALSE)
  rs   <- DBI::dbSendQuery(con, "SELECT * FROM mtcars")
  
  df <- mtcars
  rownames(df) <- NULL
  
  test_connection(rs, df)
  
})


