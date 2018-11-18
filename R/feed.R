#' Repeatedly iterate over `data.frame` or `connection` in chunks.
#' 
#' @md
#' @description
#' Returns a function that repeats the following cycle:  
#' iteratively return `chunk_size` number of rows from `data` until data is exhausted;
#' then return `NULL` once.
#' 
#' @param data `data.frame`, `file`, `gzfile`, or `url`connection`
#' @param chunk_size number of chunks to return with each iteration. overrides
#'   `nrow` parameter of `read.table` when `data` is a connection.
#' @param header logical, when TRUE colnames are determined from first row
#'   of connection. if FALSE `col_names` must be provided.
#' @param col_names, character vector. override `col_names` in first
#'   row of connection when `header`is TRUE, or provide colnames
#'   if `header` is FALSE.
#' @param ... passed through to `read.table`. note that `nrow`, and 
#'   `col.names` are ignored in favor of `chunk_size` and `col_names`
#' @details `oomfeed` is a closure that creates a function for 
#'   returning `chunk_size` number of rows from `data` until all
#'   rows have been returned.  It will then return `NULL` once. It will repeat
#'   this cycle ad-infinitum. When data is a `connection` it will recreate and open
#'   the connection with each cycle.  It reads from `connection` objects using
#'   `read.table`.  Additional parameters for `read.table` should be passed
#'   in via `...`.
#' @export
oomfeed <- function(data, chunk_size, ...){
  UseMethod("oomfeed")
}
setGeneric("oomfeed")


#' @export
oomfeed.data.frame <- function(data, chunk_size, ...) {

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
    cursor <<- cursor + min(chunk_size, n - cursor)

    if(cursor == n) {
      reset <<- TRUE
    }

    data[start:cursor, ]

  }
}


#' @export
oomfeed.connection <- function(data,
                               chunk_size,
                               header    = TRUE,
                               col_names = NULL, ...) {

  if(!header & is.null(col_names)) {
    stop(cat("col_names must be provided if header is FALSE"))
  }
  
  data_summary <- summary(data)
  close(data)
  
  first_iter   <- TRUE
  reset        <- FALSE

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
    
    if(first_iter & header) {
      rval <- read.table(data, nrows = chunk_size, header = TRUE)
      col_names  <<- colnames(rval)
    } else {
      rval <- read.table(data, nrows = chunk_size, col.names = col_names)
    }
    
    first_iter <<- FALSE
    
    if(nrow(rval) == 0) {
      close(data)
      reset <<- TRUE
      data  <<- NULL
      return(NULL)
    }
    
    rval
  }
}


oomfeed.DBIResult <- function(data,
                              chunk_size,
                              ...) {
  reset  <- FALSE
  con    <- data@conn
  query  <- data@sql
  rval_n <- 0
  
  DBI::dbClearResult(data)
  data <- DBI::dbSendQuery(con, query)
  
  function() {
    
    if(reset) {
      if(DBI::dbIsValid(data)) {
        DBI::dbClearResult(data)
      }
      data  <<- DBI::dbSendQuery(con, query)
      reset <<- FALSE
    }
    
    if(!DBI::dbHasCompleted(data)) {
      rval   <- DBI::dbFetch(data, chunk_size)
      rval_n <- nrow(rval)
    } else {
      rval_n <- 0
    }
    
    if(rval_n == 0) {
      reset <<- TRUE
      DBI::dbClearResult(data)
      return(NULL)
    }
    
    rval
  }
  
}

