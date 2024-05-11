#' @export
call_eval0 <- function (x, ..., data = NULL, env = caller_env()) {
  stopifnot(is_call(x))
  eval_tidy(expr = x, data = data, env = env)
}
call_eval <- function (x, ..., data = NULL, env = caller_env()) {
  x <- encall(x)
  if(is_vector(x)){
    map(x, call_eval0, data = data, env = env)
  } else {
    call_eval0(x)
  }
}
