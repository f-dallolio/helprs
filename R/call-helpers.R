# call_add_ns -----

#' Add Namespace to a call.
#'
#' @inheritParams call_replace
#'
# #' @return
#' @export
#'
# #'  @examples
call_add_ns <- function(x, ..., .fn = NULL, .ns = NULL, .private = NULL) {
  x <- encall0(x)

  if (is.null(.ns)) {
    if (is.null(.fn)) {
      .fn <- try(eval(x[[1]], envir = globalenv()), silent = TRUE)
    }
    if (is.function(.fn)) {
      .ns <- ns_env_name(.fn)
    } else {
      warning("Could not find a namespace. Returning the input as call.")
      return(x)
    }
  } else if (is.environment(.ns)) {
    .ns <- ns_env_name(.ns)
  } else {
    warning("Could not find a namespace. Returning the input as call.")
    return(x)
  }

  if (is_string(.ns)) {
    .ns <- str2lang(.ns)
  }
  .fn <- str2lang(call_name(x))

  if (.private) {
    op <- ":::"
  } else {
    op <- "::"
  }

  x0 <- as.list(x)
  x0[[1]] <- call(op, .ns, .fn)

  as.call(x0)
}



# call_replace -----

#' Replace call components
#'
#' @inheritParams rlang::call2
#' @param x an R object to convert into a simple call. It is passed to `encall0`.
#' @param ... must remain empty.
#' @param .args a list.
#'
#' @return a simple call.
#' @name call_replace
NULL

#' @rdname call_replace
#' @export
call_replace <- function(x,
                         ...,
                         .fn = call_name(x),
                         .args = call_args(x),
                         .ns = call_ns(x)) {
  x <- encall0(x)
  stopifnot(is_call_simple(x))
  call2(.fn = .fn, !!!.args, .ns = .ns)
}
#' @rdname call_replace
#' @export
call_replace_fn <- function(x, .fn) {
  call_replace(x = x, .fn = .fn)
}
#' @rdname call_replace
#' @export
call_replace_ns <- function(x, .ns) {
  call_replace(x = x, .ns = .ns)
}
#' @rdname call_replace
#' @export
call_add_ns <- function(x, .ns) {
  call_replace_ns(x = x, .ns = .ns)
}





# call_text -----

#' Deparse Call
#'
#' @param x a call.
#' @param ... empty.
#' @param .simplify a logical.
#'
#' @return a string.
#' @export
call_text <- function(x, ..., .simplify = FALSE) {
  stopifnot(is_call(x))
  flag <- is_empty(call_args(x))
  if (.simplify && flag) {
    deparse(x[[1]])
  } else {
    deparse(x)
  }
}


# encall -----

#' Objects as calls
#'
#' @inheritParams rlang::enexprs
#' @param x a string, a symbol, or a call.
#' @param ... strings or expressions to defuse.
#'
#' @return namespaced call(s).
#' @name encall
NULL

#' @rdname encall
#' @export
encall0 <- function(x) {
  if (is_string(x)) {
    x <- str2lang(x)
  } else if (is.function(x)) {
    x <- str2lang(fn_name(x, with_ns = FALSE))
  }
  stopifnot(is_symbolic(x))
  if (is_call_simple(x)) {
    return(x)
  }
  as.call(list(x))
}

#' @rdname encall
#' @export
encall_replace <- function(x, ..., .args = NULL, .ns = NULL) {
  check_dots_empty()
  call_replace(x = encall0(x), .args = .args, .ns = .ns)
}

#' @rdname encall
#' @export
encall <- function(x, .simplify = TRUE, .named = TRUE) {
  out <- map(as_list0(x), encall0)
  out
}

#' @rdname encall
#' @export
encalls <- function(...,
                    .named = TRUE,
                    .ignore_empty = c("trailing", "none", "all"),
                    .ignore_null = c("none", "all"),
                    .unquote_names = TRUE,
                    .homonyms = c("keep", "first", "last", "error"),
                    .check_assign = FALSE) {
  x <- enexprs(...,
    .named = .named,
    .ignore_empty = .ignore_empty,
    .ignore_null = .ignore_null,
    .unquote_names = .unquote_names,
    .homonyms = .homonyms,
    .check_assign = .check_assign
  )
  chr_flag <- map_lgl(x, is_string)
  names(x)[chr_flag] <- x[chr_flag]
  map(x, encall0)
}


# call_eval -----

#' Evaluate a call or a list of calls
#'
#' @param x a call (`call_eval0`) or an R object or list of objects (`call_eval0`) that will be passed to `encall(x)`.
#' @param ... must remain empty.
#' @param data a data frame, or named list or vector.
#' @param env the environment in which to evaluate `x`.
#'
#' @name call_eval
NULL
#'
#' @rdname call_eval
#' @export
call_eval0 <- function(x, ..., data = NULL, env = caller_env()) {
  check_dots_empty()
  stopifnot(is_call(x))
  eval_tidy(expr = x, data = data, env = env)
}
#' @rdname call_eval
#' @export
call_eval <- function(x, ..., data = NULL, env = caller_env()) {
  check_dots_empty()
  x <- encall(x, .simplify = FALSE)
  out <- map(x, call_eval0, data = data, env = env)
  out
}
