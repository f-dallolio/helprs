#' Get functions from function names and namespace
#'
#' @param x a string (`get_fn`) or a character vector (`get_fns`).
#' @param ns the namespace name.
#' @param ifnotfound the return value of `get_fn*(x, *)` when x does not exist.
#' @param names_only logical. If `TRUE`, the function returns only the names of the functions.
#' @param export_only logical. If `TRUE`, the function returns only the names of the functions.
#'
#' @return a function or list of functions.
#'
#' @name get-functions
NULL

#' @rdname get-functions
#' @export
get_fn <- function(x,
                   ns = NULL,
                   ifnotfound = NULL) {
  stopifnot(is_string(x))
  if (is.null(ns)) {
    envir <- pos.to.env(-1)
  } else if (is.environment(ns)) {
    envir <- ns
    ns <- environmentName(ns)
  } else {
    envir <- asNamespace(ns)
  }
  out <- get0(x = x, envir = envir, mode = "function", ifnotfound = ifnotfound)
  out <- as_closure(out)
  if(is.null(ns)){
    return(out)
  }
  environment(out) <- asNamespace(ns)
  out
}
#' @rdname get-functions
#' @export
get_fns <- function(x = NULL,
                    ...,
                    ns = NULL,
                    ifnotfound = NULL,
                    names_only = FALSE,
                    exports_only = FALSE) {
  check_dots_empty()
  if (is.null(x)) {
    if (exports_only) {
      x <- getNamespaceExports(ns)
    } else {
      x <- names(asNamespace(ns))
    }
    if (names_only) {
      return(x)
    }
  }
  map(setNames(x, x), get_fn, ns = ns, ifnotfound = ifnotfound)
}
#' @rdname get-functions
#' @export
get_ns_fns <- function(ns,
                       ...,
                       ifnotfound = NULL,
                       exports_only = FALSE){
  if(exports_only){
    x <- ns_exports(ns)
  } else {
    x <- ns_names(ns)
  }
  out <- map(x,
             get0,
             envir = asNamespace(ns),
             ifnotfound = ifnotfound)
  names(out) <- x
  fns_out <- keep(out, is.function)
  fns_out <- modify_if(fns_out, fn_is_builtin, as_closure)
  modify(fns_out, fn_set_ns, "rlang")
}
#' @rdname get-functions
#' @export
get_export_fns <- function(ns = NULL) {
  get_ns_fns(ns = ns, exports_only = TRUE)
}
#' @rdname get-functions
#' @export
ns_names <- function (ns,
                      exports_only = FALSE) {
  if(exports_only){
    getNamespaceExports(asNamespace(ns))
  } else {
    names(asNamespace(ns))
  }
}
#' @rdname get-functions
#' @export
ns_exports <- function (ns) {
  ns_names(ns, exports_only = TRUE)
}
