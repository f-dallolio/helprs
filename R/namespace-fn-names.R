#' Get names of functions in a namespace.
#'
#' @param ns a string indicating the name of a namespace.
#' @param exports_only logical. If TRUE the function retrieves the names of exported functions only. If FALSE, the function retrieves the names of all functions in a namespace.
#'
#' @return a character vector.
#'
#' @name namespace-fn-names
NULL
#' @name namespace-fn-names
#' @export
#'
# #' @examples
ns_fn_names <- function (ns,
                         exports_only = FALSE) {
  envir <- asNamespace(ns)
  if(exports_only){
    out <- getNamespaceExports(ns)
  } else {
    out <- names(envir)
  }
  ns_out <- map(out, get0, envir = envir)
  fn_flag <- map_lgl(ns_out, is.function)
  out[fn_flag]
}
#' @rdname namespace-fn-names
#' @export
ns_exports_names <- function (ns) {
  ns_fn_names(ns, exports_only = TRUE)
}
#' @rdname namespace-fn-names
#' @export
ns_private_names <- function (ns) {
  setdiff(ns_fn_names(ns), ns_fn_names(ns, exports_only = TRUE))
}
