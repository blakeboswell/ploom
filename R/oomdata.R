

#' Internal. Construct `oomdata` record
#' 
#' @keywords internal
init_oomdata <- function() {

  list(
    completed_iter  = 0.,
    completed_chunk = 0.,
    obs_seen        = 0.,
    chunk_per_iter  = NA_real_,
    obs_per_iter    = NA_real_
  )

}


#' Internal. Update `oomdata` function record
#' 
#' @details Keeps track of number of iterations, chunks, and
#' observations that have been processed.
#' 
#' @param obj An `oomdata` function.
#' @param chunk_size The number of observations in the previous chunk.
#' @param complete_iteration Signals that a pass over the data is complete.
#' 
#' @keywords internal
update_oomdata <- function(obj, chunk_size, complete_iteration) {
  
  if(complete_iteration) {
  
    if(is.na(obj$chunk_per_iter)) {
      obj$chunk_per_iter <- obj$completed_chunk
    }
    
    if(is.na(obj$obs_per_iter)) {
      obj$obs_per_iter <- obj$obs_seen
    }
    
    obj$obs_seen   <- 0
    obj$completed_chunk <- 0
    obj$completed_iter  <- obj$completed_iter + 1
    
  } else {
    obj$completed_chunk <- obj$completed_chunk + 1
    obj$obs_seen   <- obj$obs_seen + chunk_size
  }
  
  obj
  
}


#' Internal. Extract information from `oomdata` function
#' 
#' @param obj An `oomdata` function.
#' 
#' @keywords internal
get_oomdata <- function(obj) {
  get("record", environment(obj))
}



#' @export
#' @method print oomdata
print.oomdata <- function(x, ...) {
  
  x <- tidy(x)
  
  cat("\n# oomdata function\n")
  
  cat("\n  ",
      paste(x$completed_iter, sep = "\n", collapse = "\n"),
      "completed iterations having",
      x$chunk_per_iter,
      "chunks and",
      x$obs_per_iter,
      "observations.",
      sep = " ")
  
  cat("\n  ",
      paste(x$obs_seen, sep = "\n", collapse = "\n"),
      "observations seen over",
      x$completed_chunk,
      "completed chunks in current iteration.",
      sep = " ")
  
  cat("\n\n")
  
  invisible(x)
  
}
