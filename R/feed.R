
#' @export
oomfeed <- function(obj, chunksize, ...){
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

  reset <- FALSE
  deats <- summary(data)
  close(data) # create fresh connection below
  
  conn_fxn <- function(class_name){
    switch(
      class_name,
      "file"        = file,
      "url-libcurl" = url,
      "gzfile"      = gzfile,
      "bzfile"      = bzfile,
      "xzfile"      = xzfile,
      NULL
    )
  }
  
  if(is.null(fxn <- conn_fxn(deats$class))) {
    stop(cat("oomfeed does not support connection type ", deats$class))
  }
  
  data <- fxn(deats$description, open = deats$mode)
  
  function() {
    
    if(reset) {
      if(!is.null(data)) {
        close(data)
      }
      data  <<- fxn(deats$description, open = deats$mode)
      reset <<- FALSE
    }
    
    rval <- read.table(data, nrows = chunksize, ...)
    
    if(nrow(rval) == 0) {
      close(data)
      reset <<- TRUE
      data  <<- NULL
      return(NULL)
    }
    
    rval
  }
}
