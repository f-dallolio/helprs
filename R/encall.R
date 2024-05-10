#' Objects as calls
#'
#' @inheritParams rlang::enexprs
#' @param x a string, a symbol, or a call.
#' @param ... strings or expressions to defuse.
#'
#' @return namespaced call(s).
#' @name encall
NULL

#' @rdname encall
#' @export
encall0 <- function(x){
  if(is_string(x)) {
    x <- str2lang(x)
  }
  stopifnot(is_symbolic(x))
  if(is_call_simple(x)){
    return(x)
  }
  as.call(list(x))
}

#' @rdname encall
#' @export
encall_replace <- function(x, ..., .args = NULL, .ns = NULL){
  check_dots_empty()
  call_replace(x = encall0(x), .args = .args, .ns = .ns)
}

#' @rdname encall
#' @export
encall <- function(x, .simplify = TRUE){
  if(length(x) == 1 && .simplify) {
    encall0(x)
  } else {
    encalls(!!!x)
  }
}

#' @rdname encall
#' @export
encalls <- function (...,
                     .named = TRUE,
                     .ignore_empty = c("trailing", "none", "all"),
                     .ignore_null = c("none", "all"),
                     .unquote_names = TRUE,
                     .homonyms = c("keep", "first", "last", "error"),
                     .check_assign = FALSE) {
  x <- enexprs(...,
               .named = .named,
               .ignore_empty = .ignore_empty,
               .ignore_null = .ignore_null,
               .unquote_names = .unquote_names,
               .homonyms = .homonyms,
               .check_assign = .check_assign)
  chr_flag <- map_lgl(x, is_string)
  names(x)[chr_flag] <- x[chr_flag]
  map(x, encall0)
}

