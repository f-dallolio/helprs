#' @export
as_string2 <- function(x){
  out <- try(as_string(x), silent = TRUE)
  if(class(out) == "try-error"){
    return(deparse(x))
  }
  out
}
#' @export
is_symbol2 <- function(x){
  if(is_quosure(x)){
    x <- quo_get_expr(x)
  }
  is_ns_sym(x) || is_symbol(x)
}
#' @export
is_symbolic2 <- function(x){
  if(is_quosure(x)){
    x <- quo_get_expr(x)
  }
  is_symbolic(x)
}
#' @export
get_fn <- function (x, ns = NULL, ifnotfound = NULL){
  stopifnot(is_string(x))
  if(is.null(ns)){
    envir <- pos.to.env(-1)
  } else if (is.environment(ns)) {
    envir <- ns
  } else {
    envir <- asNamespace(ns)
  }
  get0(x = x, envir = envir, mode = "function", ifnotfound = ifnotfound)
}
#' @export
get_fns <- function (x = NULL, ..., ns = NULL, names_only = TRUE, exports_only = FALSE){
  check_dots_empty()
  if(is.null(x)){
    if(exports_only){
      x <- getNamespaceExports(ns)
    } else {
      x <- names(asNamespace(ns))
    }
    if(names_only){
      return(x)
    }
  }
  map(setNames(x, x), get_fn, ns = ns, ifnotfound = ifnotfound)
}
#' @export
get_fns_export <- function (x = NULL, ..., ns = NULL, names_only = TRUE){
  get_fns(x = x, ns = ns, names_only = names_only, exports_only = TRUE)
}
