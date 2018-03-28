  
oomfeed <- function(data,
                    chunksize = 5000) {
  
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
