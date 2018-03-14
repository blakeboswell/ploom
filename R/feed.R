  
  
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
    
    if(cursor == n){
      reset <- TRUE
    }
    
    data[start:cursor, ]
    
  }
  
}



# ihas_next <- function(it) {
#   
#   if(!is.null(it$hasNext)) {
#     return(it)
#   }
#   
#   cache    <- NULL
#   has_next <- NA
#   
#   next_el <- function() {
#     if(!has_nx()) {
#       stop("StopIteration", call. = FALSE)
#       has_next <<- NA
#       cache
#     }
#   }
#   
#   has_nx <- function() {
#     
#     if(!is.na(has_next)) {
#       return(has_next)
#     }
#     
#     tryCatch({
#       cache    <<- nextElem(it)
#       has_next <<- TRUE
#     },
#     error = function(e) {
#       if (identical(conditionMessage(e), 'StopIteration')) {
#         has_next <<- FALSE
#       } else {
#         top(e)
#       }
#     })
#     has_next
#   }
#   
#   obj <- list(
#     next_elem = next_el,
#     has_next  = has_nx
#   )
#   clas(obj) <- c('ihasNext')
#   
#   obj
#   
# }