context("test-oomfeed-con.R")

test_connection <- function(con, data_frame) {

  cursor     <- 0  
  chunk_size <- 5
  n <- nrow(data_frame)
  
  tmp <- oomfeed(con, chunk_size = chunk_size, header = TRUE)
  
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
  tmp    <- oomfeed(df, chunk_size = chunk_size)
  
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
  df  <- mtcars
  rownames(df) <- NULL
  test_connection(file("../testdata/mtcars.txt"), df)
})


test_that("gzfile", {
  df <- mtcars
  rownames(df) <- NULL
  test_connection(gzfile("../testdata/mtcars.txt.gz"), df)
})

