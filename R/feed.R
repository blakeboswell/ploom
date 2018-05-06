
#' @export
oomfeed <- function(obj, chunksize, ...){
  UseMethod("oomfeed")
}
setGeneric("oomfeed")


#' @export
oomfeed.data.frame <- function(conn, chunksize, ...) {

  reset  <- FALSE
  n      <- nrow(conn)
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

    conn[start:cursor, ]

  }

}



#' @export
oomfeed.connection <- function(conn, chunksize, ...) {

  reset <- FALSE
  deats <- summary(conn)
  
  conn_fxn <- function(class_name){
    switch(class_name, "file" = file, "url-libcurl" = url, NULL)
  }
  
  if(is.null(fxn <- conn_fxn(deats$class))) {
    stop("unsuppored connection")
  }
  
  close(conn)
  conn <- fxn(deats$description, open = deats$mode)
  
  function() {

    if(reset) {
      if(!is.null(conn)) {
        close(conn)
      }
      conn  <<- fxn(deats$description, open = deats$mode)
      reset <<- FALSE
    }

    rval <- read.table(conn, nrows = chunksize, ...)

    if(nrow(rval) == 0) {
      close(conn)
      reset <<- TRUE
      conn  <<- NULL
      return(NULL)
    }

    rval

  }
}
