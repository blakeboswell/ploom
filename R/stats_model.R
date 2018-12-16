# This is lifted directly from base `stats`.  Much of the flexibility and robustness
# of the original code is been removed to decrease processing time


#' lifted as-is from `stats`
#' 
#' @md
#' @param x variables extracted form `terms` object
#' @keywords internal
deparse2 <- function(x) {
  paste(deparse(x, width.cutoff = 500L, backtick = !is.symbol(x) && is.language(x)),
        collapse = " ")
}


#' lifted from `stats`. modified to be faster and less robust
#' 
#' @md
#' @param formula `stats::terms` object
#' @param data data.frame or list
#' @keywords internal
model_frame <- function(formula, data = NULL) {
  
  if (is.array(data)) {
    stop("'data' must be a data.frame, not a matrix or an array")
  }
  
  env      <- environment(formula)
  vars     <- attr(formula, "variables")
  predvars <- attr(formula, "predvars")
  
  if(is.null(predvars)) {
    predvars <- vars
  }
  
  varnames  <- vapply(vars, deparse2, " ")[-1L]
  variables <- eval(predvars, data, env)

  if(is.null(attr(formula, "predvars"))) {
    for (i in seq_along(varnames)) {
      predvars[[i+1L]] <- makepredictcall(variables[[i]], vars[[i+1L]])
    }
    attr(formula, "predvars") <- predvars
  }
    
  extras     <- list()
  extranames <- NULL
  subset     <- NULL
  rownames   <- NULL
  
  data       <- .External2(getFromNamespace("C_modelframe", "stats"),
                           formula,
                           rownames,
                           variables,
                           varnames,
                           extras,
                           extranames,
                           subset,
                           na.fail)

  attr(formula, "dataClasses") <- vapply(data, .MFclass, "")
  attr(data, "terms")          <- formula
  data
    
}


#' lifted from `stats`. modified to be faster and less robust
#' 
#' @md
#' @param formula `stats::terms` object
#' @param data data.frame or list
#' @keywords internal
model_matrix <- function(terms, data) {
  
  reorder <- match(vapply(attr(terms, "variables"), deparse2, "")[-1L],
                   names(data))
  
  if (anyNA(reorder)) {
    stop("model frame and formula mismatch in model.matrix()")
  }
  
  if(!identical(reorder, seq_len(ncol(data)))) {
    data <- data[, reorder, drop = FALSE]
  }

  int <- attr(terms, "response")
  
  if(length(data)) {
    
    contr.funs <- as.character(getOption("contrasts"))
    namD       <- names(data)
    
    ## turn any character columns into factors
    for(i in namD) {
      if(is.character(data[[i]])) {
        data[[i]] <- factor(data[[i]])
      }
    }

    isF      <- vapply(data, function(x) is.factor(x) || is.logical(x), NA)
    isF[int] <- FALSE
    isOF     <- vapply(data, is.ordered, NA)
    
    for(nn in namD[isF]) {  # drop response
      if(is.null(attr(data[[nn]], "contrasts"))) {
        contrasts(data[[nn]]) <- contr.funs[1 + isOF[nn]]
      }
    }

  } else { #  no rhs terms ('~1', or '~0'): internal model.matrix needs some variable
    isF         <- FALSE
    data[["x"]] <- raw(nrow(data))
  }
  
  ans <- .External2(getFromNamespace("C_modelmatrix", "stats"),
                    terms, data)
  
  if(any(isF)) {
    attr(ans, "contrasts") <- lapply(data[isF], attr, "contrasts")
  }
    
  ans

}


#' lifted from `stats`. modified to be faster and less robust
#' 
#' @md
#' @param x `stats::formula` object
#' @keywords internal
model_offset <- function(x) {
  
  offsets <- attr(attr(x, "terms"), "offset")
  
  if(length(offsets)) {
    
    ans <- x$"(offset)"
    
    if (is.null(ans)) {
      ans <- 0
    }
    
    for(i in offsets) {
      ans <- ans + x[[i]]
    }
    
    ans
    
  } else {
    ans <- x$"(offset)"
  }
  
  if(!is.null(ans) && !is.numeric(ans)) {
    stop("'offset' must be numeric")
  }
  
  ans

}


#' lifted from `stats`. modified to be faster and less robust
#' 
#' @md
#' @param data data.frame or list
#' @param type response type
#' @keywords internal
model_response <- function(data, type = "any") {
  
  
  if(!attr(attr(data, "terms"), "response")) {
    return(NULL)
  }
  
  if (is.list(data) | is.data.frame(data)) {
    
    v <- data[[1L]]
    
    if (type == "numeric" && is.factor(v)) {
      warning('using type = "numeric" with a factor response will be ignored')
    } else if (type == "numeric" | type == "double") {
      storage.mode(v) <- "double"
    } else if (type != "any") {
      stop("invalid response type")
    }
    
    if (is.matrix(v) && ncol(v) == 1L) {
      dim(v) <- NULL
    }
    
    rows <- attr(data, "row.names")
    
    if (nrows <- length(rows)) {
      
      if (length(v) == nrows) {
        names(v) <- rows
      } else if (length(dd <- dim(v)) == 2L) {
        if (dd[1L] == nrows && !length((dn <- dimnames(v))[[1L]])) {
          dimnames(v) <- list(rows, dn[[2L]])
        }
      }
    }
    
    return(v)
    
  } else {
    stop("invalid 'data' argument")
  }
    
}


#' extract elements from data for updating
#' 
#' @param obj ploom model object
#' @param data `data.frame`
#' @keywords internal
unpack_oomchunk <- function(obj, data) {
  
  chunk_data   <- model_frame(obj$terms, data)
  chunk_assign <- attr(chunk_data, "assign")
  
  if(!is.null(obj$assign)) {
    if (!identical(obj$assign, chunk_assign)) {
      stop("model matrices incompatible")
    }
  }
  
  if(is.null(chunk_offset <- model_offset(chunk_data))) {
    chunk_offset <- 0.0
  }
  
  chunk_response <- model_response(chunk_data) - chunk_offset
  chunk_data     <- model_matrix(obj$terms, chunk_data)
  
  p <- ncol(chunk_data)
  n <- nrow(chunk_data)
  
  if(is.null(obj$weights)) {
    chunk_weights <- rep(1.0, n)
  } else {
    chunk_weights <- model_frame(terms(obj$weights), data)[[1]]
  }
  
  list(
    data     = chunk_data,
    n        = n,
    p        = p,
    response = chunk_response,
    weights  = chunk_weights,
    offset   = chunk_offset,
    assign   = chunk_assign
  )
  
}

