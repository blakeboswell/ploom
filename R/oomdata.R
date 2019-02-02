
#' Iterate over `tibble()`, `DBIResult` or `connection()` in chunks.
#' 
#' @md
#' @description
#' Returns a function that repeats the following cycle:  
#' iteratively return `chunk_size` number of rows from `data` until data is exhausted;
#' then return `NULL` once.
#' 
#' @param data [tibble()], `DBIResult`, or [connection()] object
#' @param chunk_size number of chunks to return with each iteration
#' @param header logical, when TRUE colnames are determined from first row
#'   of connection. if FALSE `col_names` must be provided.
#' @param col_names character vector. override `col_names` in first
#'   row of connection when `header`is TRUE, or provide colnames
#'   if `header` is FALSE.
#' @param ... ignored
#' @details `oomdata_*` functions are closures that return functions that
#'   iteratively return `chunk_size` number of rows from `data` until all
#'   rows have been returned. They will then return `NULL` once. They repeat
#'   this cycle ad-infinitum. 
#'   
#' @export
#' @name oomdata_dbi
#' @examples \donttest{
#' # `oomdata_tbl()` returns a function that when called will
#' # return `chunk_size` rows from a `tbl_df()`
#' chunks <- oomdata_tbl(mtcars, chunk_size = 16)
#' 
#' nrow(chunks())
#' nrow(chunks())
#' 
#' # when the data is exhausted the `oomdata_tbl()` function
#' # will return NULL once.
#' chunks()
#' 
#' # subsequent calls restart the cycle
#' nrow(chunks())
#' nrow(chunks())
#' chunks()
#' 
#' # `while` loops are useful for iterating over 
#' # `oomfeed` functions
#' while(!is.null(chunk <- chunks())){
#'   print(nrow(chunk))
#' }
#' 
#' # `oomdata_dbi()` returns a function that when called will
#' # return `chunk_size` rows from a query result set.
#' con <- DBI::dbConnect(RSQLite::SQLite(), path = ":dbname:")
#' dplyr::copy_to(con, mtcars, "mtcars", temporary = FALSE)
#' rs  <- DBI::dbSendQuery(con, "SELECT mpg, cyl, disp FROM mtcars")
#' 
#' chunks <- oomdata_dbi(rs, 16)
#' 
#' while(!is.null(chunk <- chunks())){
#'   print(nrow(chunk))
#' }
#'
#' # ploom model functions automatically iterate over `oomdata` until 
#' # the source is exhausted (`oomlm()`, `oomlm_robust()`) or until 
#' # IRLS convergence (`oomglm()``)
#' x <- fit(oomlm(mpg ~ cyl + disp), chunks)
#' y <- fit(oomglm(mpg ~ cyl + disp), chunks)
#'
#' coef(x)
#' coef(y)
#' 
#' }
NULL


#' @export
#' @name oomdata_dbi
oomdata_tbl <- function(data, chunk_size, ...) {

  reset  <- FALSE
  n      <- nrow(data)
  cursor <- 0

  fn <- function() {

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
  
  class(fn) <- c("oomdata_tbl", "oomdata", class(fn))
  fn
  
}


#' @export
#' @name oomdata_dbi
oomdata_dbi <- function(data, chunk_size, ...) {
  
  reset  <- FALSE
  con    <- data@conn
  query  <- data@sql
  rval_n <- 0
  
  DBI::dbClearResult(data)
  data <- DBI::dbSendQuery(con, query)
  
  fn <- function() {
    
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
  
  class(fn) <- c("oomdata_dbi", "oomdata", class(fn))
  fn
  
}


#' @export
#' @name oomdata_dbi
oomdata_con <- function(data, chunk_size,
                        header    = TRUE,
                        col_names = NULL, ...) {

  if(!header & is.null(col_names)) {
    stop(cat("col_names must be provided if header is FALSE"))
  }

  data_summary <- summary(data)
  close(data)

  first_iter   <- TRUE
  reset        <- FALSE

  con_switch <- function(class_name){
    switch(
      class_name,
      "file"        = file,
      "url-libcurl" = url,
      "gzfile"      = gzfile,
      NULL
    )
  }

  if(is.null(con_fn <- con_switch(data_summary$class))) {
    stop(paste0("oomdata does not support connection type ", data_summary$class))
  }

  data <- con_fn(data_summary$description)
  open(data)

  fn <- function() {

    if(reset) {
      data  <<- con_fn(data_summary$description)
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

  class(fn) <- c("oomdata_con", "oomdata", class(fn))
  fn

}
