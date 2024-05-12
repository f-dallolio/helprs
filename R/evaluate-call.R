#' Evaluate a call or a list of calls
#'
#' @param x a call (`call_eval0`) or an R object or list of objects (`call_eval0`) that will be passed to `encall(x)`.
#' @param ... must remain empty.
#' @param data a data frame, or named list or vector.
#' @param env the environment in which to evaluate `x`.
#'
#' @name evaluate-call
NULL
#'
#' @rdname evaluate-call
#' @export
call_eval0 <- function (x, ..., data = NULL, env = caller_env()) {
  check_dots_empty()
  stopifnot(is_call(x))
  eval_tidy(expr = x, data = data, env = env)
}
#' @rdname evaluate-call
#' @export
call_eval <- function (x, ..., data = NULL, env = caller_env()) {
  check_dots_empty()
  x <- encall(x, .simplify = FALSE)
  out <- map(x, call_eval0, data = data, env = env)
  out
}
