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
as_list0 <- function(x){
  if(is_vector(x)){
    as.list(x)
  } else {
    list(x)
  }
}

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

special_fns <- function(internal = FALSE){
  builtins2(internal = FALSE, type = "special")
}


builtin_fns <- function(internal = FALSE){
  builtins2(internal = FALSE, type = "builtin")
}


fn_type <- function(fn){
  stopifnot(is_function(fn))
  typeof(fn)
}
fn_is_closure <- function(fn){
  fn_type(fn) == "closure"
}
fn_is_special <- function(fn){
  fn_type(fn) == "special"
}
fn_is_builtin <- function(fn){
  fn_type(fn) == "builtin"
}


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

fn_set_name <- function(fn){
  setNames(list(fn), fn_name(fn))
}

fn_set_ns <- function(fn, ns){
  stopifnot(is_function(fn))
  environment(fn) <- asNamespace(ns)
  fn
}


fn_names <- function(..., with_ns = TRUE){
  x <- list2(...)
  out <- character(length(x))
  fn_flag <- map_lgl(x, is.function)
  out[fn_flag] <- map_chr(x[fn_flag], fn_name,
                          names_only = TRUE, with_ns = with_ns)
  out
}
