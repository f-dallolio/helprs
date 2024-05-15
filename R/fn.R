#' Extend rlang's fn functions
#'
#' @param fn a function.
#' @param with_ns a logical. If `TRUE`, `fn_name(s)(*)` return  the name(s) of the functions as input including their namespace.
#' @param ns a string. The name of a namespace.
#'
#' @name fn
NULL
#' @rdname fn
#' @export
fn_type <- function(fn) {
  stopifnot(is_function(fn))
  typeof(fn)
}
#' @rdname fn
#' @export
fn_is_closure <- function(fn) {
  fn_type(fn) == "closure"
}
#' @rdname fn
#' @export
fn_is_special <- function(fn) {
  fn_type(fn) == "special"
}
#' @rdname fn
#' @export
fn_is_builtin_as_closure <- function(fn){
  if(rlang::is_closure(fn)){
    body <- body(fn)
    if(rlang::is_symbol(body) | is.character(body)){
      return(FALSE)
    }
    typeof(body[[1]]) == "builtin"
  } else {
    FALSE
  }
}
#' @rdname fn
#' @export
fn_is_builtin <- function(fn){
  fn_is_builtin_as_closure(fn) || typeof(fn) == "builtin"
}
#' @rdname fn
#' @export
fn_revert_builtin <- function(fn){
  if(fn_is_builtin_as_closure(fn)){
    body <- body(fn)
    if(rlang::is_symbol(body) || is.character(body)){
      return(fn)
    }
    body[[1]]
  } else {
    fn
  }
}
#' @rdname fn
#' @export
fn_reenv <- function(fn, env = globalenv()){
  x <- map(as_list0(fn), fn_revert_builtin)
  out <- map_if(x, Negate(fn_is_builtin), `environment<-`, env)
  if(length(out) == 1){
    out[[1]]
  } else {
    out
  }
}
#' @rdname fn
#' @export
fn_name <- function(x, fns = NULL, ..., with_ns = TRUE, env = globalenv()){
  if(is.null(fns)){
    envir <- environment(x)
    if(is.null(envir)){
      ns <- "base"
      envir <- asNamespace(ns)
    } else {
      ns <- environmentName(envir)
    }
    fns <- get_fns(ns)
  }
  x <- fn_reenv(x, env = env)
  fns <- fn_reenv(fns, env = env)
  out <- keep(fns, identical, x = x)
  nms <- names(out)
  nms <- nms[nms != ".Last.value"]
  if(length(nms) > 1){
    nms <- nms[[1]]
  }
  if(with_ns){
    if(nms %in% getNamespaceExports(ns)){
      op <- "::"
    } else {
      op <- ":::"
    }
    paste0(ns, op, nms)
  } else {
    nms
  }
}
# fn_name <- function(fn, with_ns =TRUE, .first_out = TRUE){
#   envir <- environment(fn)
#   if(is.null(envir)){
#     ns <- "base"
#     envir <- asNamespace(ns)
#   } else {
#     ns <- environmentName(envir)
#   }
#   ns_fn_names <- c(unclass(lsf.str(envir = envir)))
#   ns_fns <- rlang::env_get_list(envir, ns_fn_names)
#   fn2 <- purrr::keep(ns_fns, rlang::is_reference, x = fn)
#   nm <- names(fn2)
#   if(.first_out && length(nm) >1){
#     nm <- nm[[1]]
#   }
#   if(ns == "R_GlobalEnv" || !with_ns){ return(nm) }
#   # if(ns == "base"){ return(paste0("base::", nm)) }
#   if(nm %in% getNamespaceExports(envir)){
#     op <- "::"
#   } else {
#     op <- ":::"
#   }
#   sprintf("%s%s%s", ns, op, nm)
# }
#' @rdname fn
#' @export
fn_name_no_ns <- function(fn){
  fn_name(fn, with_ns = FALSE)
}
#' @rdname fn
#' @export
fn_name_with_ns <- function(fn){
  fn_name(fn, with_ns = TRUE)
}
#' @rdname fn
#' @export
fn_sym <- function(fn, with_ns = TRUE){
  str2lang(fn_name(fn, with_ns = TRUE))
}
#' @rdname fn
#' @export
fn_sym_no_ns <- function(fn){
  str2lang(fn_name(fn, with_ns = FALSE))
}
#' @rdname fn
#' @export
fn_sym_with_ns <- function(fn){
  str2lang(fn_name(fn, with_ns = TRUE))
}
#' @rdname fn
#' @export
fn_names <- function(..., with_ns = TRUE) {
  x <- list2(...)
  map_chr(x, fn_name)
}
#' @rdname fn
#' @export
fn_set_name <- function(fn) {
  setNames(list(fn), fn_name(fn))
}
#' @rdname fn
#' @export
fn_ns <- function(fn) {
  stopifnot(is.function(fn))
  env <- try(ns_env(fn), silent = TRUE)
  if (isNamespace(env)) {
    ns_env_name(env)
  } else {
    ""
  }
}
#' @rdname fn
#' @export
fn_set_ns <- function(fn, ns) {
  stopifnot(is_function(fn))
  check_installed(ns)
  environment(fn) <- asNamespace(ns)
  fn
}
#' @rdname fn
#' @export
fn_in_ns <- function(fn, ns) {
  stopifnot("`ns` must be a string." = is_string(ns))
  check_installed(ns)
  if (is.function(fn)) {
    nm <- fn_name(fn)
  } else if (is_string(fn)) {
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
fn_is_private <- function(fn, ns = NULL) {
  if (is.null(ns)) {
    env <- environment(fn)
    if (is_reference(env, global_env())) {
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
fn_is_export <- function(fn, ns = NULL) {
  if (is.null(ns)) {
    env <- environment(fn)
    if (is_reference(env, global_env())) {
      return(FALSE)
    }
    ns <- environmentName(env)
  } else {
    check_installed(ns)
  }
  nm <- fn_name(fn, with_ns = FALSE)
  nm %in% ns_exports_names(ns)
}
#' @rdname fn
#' @export
fn_fmls_syms0 <- function(fn, no_dots = TRUE){
  nms <- fn_fmls_syms(fn)
  if(no_dots){
    dots_flag <- names(nms) != ""
    return(nms[dots_flag])
  }
  nms
}

