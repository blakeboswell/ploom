

#' Repeatedly iterate over `data.frame` or `connection` in chunks.
#' 
#' @md
#' @description
#' Returns a function that repeats the following cycle:  
#' iteratively return `chunksize` number of rows from `data` until data is exhausted;
#' then return `NULL` once.
#' 
#' @param data `data.frame`, `file`, `gzfile`, or `url`connection`
#' @param chunksize number of chunks to return with each iteration. overrides
#'   `nrow` parameter of `read.table` when `data` is a connection
#' @param ... passed through to `read.table`
#' @details `oomfeed` is a closure that creates a function for 
#' returning `chunksize` number of rows from `data` until all
#' rows have been returned.  It will then return `NULL` once.  Then
#' it will return `chunksize` number of rows from `data`.  It will repeat
#' this cycle ad-infinitum. When data is a `connection` it will recreate and open
#' the connection with each cycle.  It reads from `connection` objects using
#' `read.table`.  Additional parameters for `read.table` should be passed
#' in via `...`.
#' @export
oomfeed <- function(data, chunksize, ...){
  UseMethod("oomfeed")
}
setGeneric("oomfeed")


#' @export
oomfeed.data.frame <- function(data, chunksize, ...) {

  reset  <- FALSE
  n      <- nrow(data)
  cursor <- 0

  function() {

    if (reset) {
      cursor <<- 0
      reset  <<- FALSE
      return(NULL)
    }

    if (cursor >= n) {
      return(NULL)
    }

    start  <- cursor + 1
    cursor <<- cursor + min(chunksize, n - cursor)

    if(cursor == n) {
      reset <<- TRUE
    }

    data[start:cursor, ]

  }

}


#' @export
oomfeed.connection <- function(data, chunksize, ...) {

  data_summary <- summary(data)
  if(isOpen(data)) {
    close(data)  
  }
  
  first_iter   <- TRUE
  reset        <- FALSE
  col_names    <- NULL

  conn_fxn <- function(class_name){
    switch(
      class_name,
      "file"        = file,
      "url-libcurl" = url,
      "gzfile"      = gzfile,
      NULL
    )
  }
  
  if(is.null(fxn <- conn_fxn(data_summary$class))) {
    stop(cat("oomfeed does not support connection type ", data_summary$class))
  }
  
  data <- fxn(data_summary$description)
  open(data)
  
  function() {
    
    if(reset) {
      data  <<- fxn(data_summary$description)
      open(data)
      first_iter <<- TRUE
      reset      <<- FALSE
    }
    
    if(first_iter) {
      rval <- read.table(data, nrows = chunksize, header = TRUE)
      col_names  <<- colnames(rval)
      first_iter <<- FALSE
    } else {
      rval <- read.table(data, nrows = chunksize, col.names = col_names)
    }
    
    if(nrow(rval) == 0) {
      close(data)
      reset <<- TRUE
      data  <<- NULL
      return(NULL)
    }
    
    rval
  }
}

