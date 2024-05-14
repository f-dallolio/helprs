#' Convert a symbolic to string
#'
#' @param x a symbol or a call.
#' @param simplify logical.
#'
#' @return a string.
#' @export
#'
lang_to_str <- function(x, simplify = TRUE){
  stopifnot(is_symbolic(x))
  x0 <- as.list(x)
  if(simplify && length(x0) == 1){
    deparse(x0[[1]])
  } else {
    deparse(x)
  }
}
