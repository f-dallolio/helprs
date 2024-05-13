# utils -----

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


# builtins2 -----

#' Extension of `base::builtins()`.
#'
#' @inheritParams base::builtins
#' @param type a string. One of `"all"`, `"closure"`, `"builtin"`, or `"sprcial"`. `"all"` is equivalent to `base::builtins()`. `"closure"` retains only the functions with type `"closure"`, `"builtin"` the functions with type `"builtin"`, and `"special"` the functions with type `"special"`.
#'
#' @return a list of functions.
#' @name builtins2
NULL
#'
#' @rdname builtins2
#' @export
builtins2 <- function(type = c("all", "closure", "builtin", "sprcial"),
                      internal = FALSE) {
  type <- match.arg(type, several.ok = TRUE)
  all_type <- "all" %in% type
  fns <- get_fns(builtins(internal = internal))
  types <- map_chr(fns, typeof)
  if (all_type) {
    return(fns)
  }
  fns[types %in% type]
}
#' @rdname builtins2
#' @export
special_fns <- function(internal = FALSE) {
  builtins2(internal = FALSE, type = "special")
}
#' @rdname builtins2
#' @export
builtin_fns <- function(internal = FALSE) {
  builtins2(internal = FALSE, type = "builtin")
}
