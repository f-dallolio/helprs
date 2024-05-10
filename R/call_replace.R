#' Replace call components
#'
#' @inheritParams rlang::call2
#' @param x an R object to convert into a simple call. It is passed to `encall0`.
#' @param ... must remain empty.
#' @param .args a list.
#'
#' @return a simple call.
#' @name call_replace
NULL

#' @rdname call_replace
#' @export
call_replace <- function(x,
                         ...,
                         .fn = call_name(x),
                         .args = call_args(x),
                         .ns = call_ns(x)){
  x <- encall0(x)
  stopifnot(is_call_simple(x))
  call2(.fn = .fn, !!!.args, .ns = .ns)
}
#' @rdname call_replace
#' @export
call_replace_fn <- function(x, .fn){
  call_replace(x = x, .fn = .fn)
}
#' @rdname call_replace
#' @export
call_replace_ns <- function(x, .ns){
  call_replace(x = x, .ns = .ns)
}
#' @rdname call_replace
#' @export
call_add_ns <- function(x, .ns){
  call_replace_ns(x = x, .ns = .ns)
}



