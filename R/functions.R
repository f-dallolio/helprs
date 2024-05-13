#' Extend rlang's fn functions
#'
#' @param fn a function.
#' @param with_ns a logical. If `TRUE`, `fn_name(s)(*)` return  the name(s) of the functions as input including their namespace.
#' @param ns a string. The name of a namespace.
#'
#' @return
#' @name fn
NULL
#' @rdname fn
#' @export
fn_type <- function(fn){
  stopifnot(is_function(fn))
  typeof(fn)
}
#' @rdname fn
#' @export
fn_is_closure <- function(fn){
  fn_type(fn) == "closure"
}
#' @rdname fn
#' @export
fn_is_special <- function(fn){
  fn_type(fn) == "special"
}
#' @rdname fn
#' @export
fn_is_builtin <- function(fn){
  fn_type(fn) == "builtin"
}
#' @rdname fn
#' @export
fn_name <- function(fn, ..., with_ns = TRUE){
  stopifnot(is_function(fn))

  fn_type <- typeof(fn)

  if(fn_type %in% c("special", "NULL")){
    msg <- sprintf("Cannot retrieve the name of a function of type `%s`", fn_type)
    stop(msg)
  }

  env <- environment(fn)

  if(is.null(env)){

    ns_fns <- builtins2(type = c("special", "builtin"))
    ns <- "base::"

  } else {

    ns0 <- environmentName(env)

    if(ns0 == "R_GlobalEnv") {

      ns_fns <- get_fns(x = ls(), names_only = FALSE)
      ns <- ""

    } else {

      ns_fns <- get_export_fns(ns = ns0)
      ns <- paste0(ns0, "::")

    }

  }

  out_fns <- keep(ns_fns, identical, fn)

  if(is_empty(out_fns)){

    ns_fns <- get_ns_fns(ns = ns0)
    ns <- paste0(ns0, ":::")
    out_fns <- keep(ns_fns, identical, fn)

  }

  if(with_ns){
    nms <- paste0(ns, names(out_fns))
    # names(out_fns) <- nms
  } else {
    nms <- names(out_fns)
  }

  nms

}
#' @rdname fn
#' @export
fn_names <- function(..., with_ns = TRUE){
  x <- list2(...)
  out <- character(length(x))
  fn_flag <- map_lgl(x, is.function)
  out[fn_flag] <- map_chr(x[fn_flag], fn_name,
                          names_only = TRUE, with_ns = with_ns)
  out
}
#' @rdname fn
#' @export
fn_set_name <- function(fn){
  setNames(list(fn), fn_name(fn))
}
#' @rdname fn
#' @export
fn_ns <- function(fn){
  stopifnot(is.function(fn))
  env <- try(ns_env(fn), silent = TRUE)
  if(isNamespace(env)){
    ns_env_name(env)
  } else {
    ""
  }
}
#' @rdname fn
#' @export
fn_set_ns <- function(fn, ns){
  stopifnot(is_function(fn))
  stopifnot(is_string(fn))
  check_installed(ns)
  environment(fn) <- asNamespace(ns)
  fn
}
#' @rdname fn
#' @export
fn_in_ns <- function(fn, ns){
  stopifnot("`ns` must be a string." = is_string(ns))
  check_installed(ns)
  if(is.function(fn)){
    nm <- fn_name(fn)
  } else if (is_string(fn)){
    nm <- fn
  } else {
    msg <- "`fn` must be a function or a string."
    stop(msg)
  }
  nms <- ns_fn_names(ns)
  nm %in% nms
}
#' @rdname fn
#' @export
fn_is_private <- function(fn, ns = NULL){
  if(is.null(ns)){
    env <- environment(fn)
    if(is_reference(env, global_env())){
      return(FALSE)
    }
    ns <- environmentName(env)
  } else {
    check_installed(ns)
  }
  nm <- fn_name(fn, with_ns = FALSE)
  nm %in% ns_private_names(ns)
}
#' @rdname fn
#' @export
fn_is_export <- function(fn, ns = NULL){
  if(is.null(ns)){
    env <- environment(fn)
    if(is_reference(env, global_env())){
      return(FALSE)
    }
    ns <- environmentName(env)
  } else {
    check_installed(ns)
  }
  nm <- fn_name(fn, with_ns = FALSE)
  nm %in% ns_exports_names(ns)
}
