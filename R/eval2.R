#' @export
eval2 <- function (x, ..., simplify = TRUE, data = NULL, env = caller_env()) {
  check_dots_empty()
  vec_flag <- is_vector(x)
  out <- map(as_list0(x), eval_tidy, data = data, env = env)
  if(simplify){
    if(vec_flag){
      return(unlist(out))
    } else {
      return(out[[1]])
    }
  }
  out
}
