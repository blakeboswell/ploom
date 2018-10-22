context("test-oomfeed-con.R")

test_connection <- function(con, data_frame) {

  cursor    <- 0  
  chunksize <- 5
  n <- nrow(data_frame)
  
  tmp <- oomfeed(con,
                 chunksize = chunksize,
                 header    = TRUE,
                 col.names = colnames(data_frame))
  
  expect(!isOpen(con))
  
  while(!is.null(x <- tmp())) {
    start  <- cursor + 1
    cursor <- cursor + min(chunksize, n - cursor)
    rownames(x) <- start:cursor
    expect_equal(x, data_frame[start:cursor, ])
  }
  
}


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
