#' Add Namespace to a call.
#'
#' @inheritParams call_replace
#'
# #' @return
#' @export
#'
# #'  @examples
call_add_ns <- function(x, ..., .ns = NULL, .fn = NULL){
  x <- encall0(x)

  if(is.null(.ns)){

    if(is.null(.fn)){
      .fn <- try(eval(x[[1]], envir = globalenv()), silent = TRUE)
    }
    if (is.function(.fn)){
      out <- rlang:::call_add_namespace(x, .fn)
      return(out)
    } else {
      warning("Could not find a namespace. Returning the input as call.")
      return(x)
    }
  } else {
    call_replace(x, .ns = .ns)
  }
}

