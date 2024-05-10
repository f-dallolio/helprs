#' Add Namespace to a call.
#'
#' @inheritParams call_replace
#'
# #' @return
#' @export
#'
# #'  @examples
call_add_ns <- function(x, ..., .fn = NULL, .ns = NULL, .private = NULL){
  x <- encall0(x)

  if(is.null(.ns)){
    if(is.null(.fn)){
      .fn <- try(eval(x[[1]], envir = globalenv()), silent = TRUE)
    }
    if (is.function(.fn)){
      .ns <- ns_env_name(.fn)
    } else {
      warning("Could not find a namespace. Returning the input as call.")
      return(x)
    }
  } else if (is.environment(.ns)) {
    .ns <- ns_env_name(.ns)
  } else {
    warning("Could not find a namespace. Returning the input as call.")
    return(x)
  }

  if(is_string(.ns)){
    .ns <- str2lang(.ns)
  }
  .fn <- str2lang(call_name(x))

  if(.private){
    op <- ":::"
  } else {
    op <- "::"
  }

  x0 <- as.list(x)
  x0[[1]] <- call(op, .ns, .fn)

  as.call(x0)
}

