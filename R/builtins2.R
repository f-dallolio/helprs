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
                      internal = FALSE){
  type <- match.arg(type, several.ok = TRUE)
  all_type <- "all" %in% type
  fns <- get_fns(builtins(internal = internal))
  types <- map_chr(fns, typeof)
  if(all_type){
    return(fns)
  }
  fns[types %in% type]
}
#' @rdname builtins2
#' @export
special_fns <- function(internal = FALSE){
  builtins2(internal = FALSE, type = "special")
}
#' @rdname builtins2
#' @export
builtin_fns <- function(internal = FALSE){
  builtins2(internal = FALSE, type = "builtin")
}
