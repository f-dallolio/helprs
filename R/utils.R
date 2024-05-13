#' @export
not_in <- function(x, vals) {
  !x %in% vals
}

#' @export
as_string2 <- function(x) {
  out <- try(as_string(x), silent = TRUE)
  if (class(out) == "try-error") {
    return(deparse(x))
  }
  out
}
#' @export
is_symbol2 <- function(x) {
  if (is_quosure(x)) {
    x <- quo_get_expr(x)
  }
  is_ns_sym(x) || is_symbol(x)
}
#' @export
is_symbolic2 <- function(x) {
  if (is_quosure(x)) {
    x <- quo_get_expr(x)
  }
  is_symbolic(x)
}
#' @export
as_list0 <- function(x) {
  if (is_vector(x)) {
    as.list(x)
  } else {
    list(x)
  }
}
