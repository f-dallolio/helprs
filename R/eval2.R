#' @export
eval2 <- function(x, ..., simplify = TRUE, data = NULL, env = caller_env()) {
  check_dots_empty()
  vec_flag <- is_vector(x)
  x <- modify_if(as_list0(x), is_string, str2lang)
  out <- map(x, eval_tidy, data = data, env = env)
  if (simplify) {
    if (vec_flag) {
      return(unlist(out))
    } else {
      return(out[[1]])
    }
  }
  out
}

# vec_classes <- function(x){
#   x <- as_list0(x)
#   map_chr(x, class)
# }
# vec_types <- function(x){
#   x <- as_list0(x)
#   map_chr(x, typeof)
# }
# is_callable()
#
#
# encall0(mean)
