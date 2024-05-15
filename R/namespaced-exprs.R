#' Test for namespaced symbols and calls
#'
#' @param x an R object.
#' @param ... must be empty.
#' @param ns a string.
#' @param private a logical or `NULL`(default). If `TRUE`, it returns `TRUE` only if the function is NOT an exported function of package `ns`. If `FALSE`, it returns `TRUE` only if the function is an exported function of package `ns`.
#' @param quote a logical. If `TRUE`, strings are first converted to expression wwith `str2lang`.
#'
#' @returns a logical.
#'
#' @name namespaced-exprs
NULL
#'
#'
#' @rdname namespaced-exprs
#' @export
is_ns_sym <- function(x, ..., ns = NULL, private = NULL, quote = TRUE) {
  if (quote && is_string(x)) {
    x <- str2lang(x)
  }
  rlang:::is_namespaced_symbol(x = x, ns = ns, private = private)
}
#'
#' @rdname namespaced-exprs
#' @export
is_ns_sym_export <- function(x, ..., ns = NULL, quote = TRUE) {
  is_ns_sym(x, ns = ns, private = FALSE, quote = quote)
}
#'
#' @rdname namespaced-exprs
#' @export
is_ns_sym_private <- function(x, ..., ns = NULL, quote = TRUE) {
  is_ns_sym(x, ns = ns, private = TRUE, quote = quote)
}
#'
#' @rdname namespaced-exprs
#' @export
is_ns_call <- function(x, ..., ns = NULL, private = NULL, quote = TRUE) {
  if (quote && is_string(x)) {
    x <- str2lang(x)
  }
  rlang:::is_namespaced_call(x = x, ns = ns, private = private)
}
#'
#' @rdname namespaced-exprs
#' @export
is_ns_call_export <- function(x, ..., ns = NULL, quote = TRUE) {
  is_ns_call(x, ns = ns, private = FALSE, quote = quote)
}
#'
#' @rdname namespaced-exprs
#' @export
is_ns_call_private <- function(x, ..., ns = NULL, quote = TRUE) {
  is_ns_call(x, ns = ns, private = TRUE, quote = quote)
}

scalar_type_sum <- function(x){
  if(is_ns_sym(x)){
    "ns_sym"
  } else if (is_ns_call(x)) {
    "ns_call"
  } else if (rlang::is_symbol(x)) {
    "sym"
  } else if (rlang::is_call(x) && !is_ns_sym(x) && !is_ns_call(x)){
    "call"
  } else {
    pillar::type_sum(x)
  }
}

type_sum <- function(x, wrapper ="<{.x}>"){
  x1 <- map_chr(x, scalar_type_sum)
  ns_sym_id <- map_lgl(x1, is_ns_sym)
  ns_call_id <- map_lgl(x1, is_ns_call)
  x1[ns_sym_id] <- "ns_sym"
  x1[ns_call_id] <- "ns_call"
   glue_wrap(x1, wrapper = wrapper)
}
