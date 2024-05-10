#' Deparse Call
#'
#' @param x a call.
#' @param ... empty.
#' @param .simplify a logical.
#'
#' @return a string.
#' @export
call_text <- function(x, ..., .simplify = FALSE){
  stopifnot(is_call(x))
  flag <- is_empty(call_args(x))
  if(.simplify && flag){
    deparse(x[[1]])
  } else {
    deparse(x)
  }
}
