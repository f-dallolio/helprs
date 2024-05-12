eval_if0 <- function(x, .p, data = NULL, env = caller_env()){
  map_if(x, .p, eval_tidy, data = data, env = env)
}
eval_if <- function(..., .p, data = NULL, env = caller_env()){
  x <- dots_list(... , .named = TRUE)
  if(is_scalar_list(x)){
    x <- dots_list(!!!x[[1]], .named = TRUE)
    str_flag <- map_lgl(x, is_string)
    names(x)[str_flag] <- x[str_flag]
  }
  eval_if0(x, .p, data = data, env = env)
}

eval2 <- function(x, data = NULL, env = caller_env()){
  not_vector <- isnot(x, .p = is_vector)
  if(not_vector){
    x <- list(x)
  }
  n <-  length(x)
  out <- rep(list(NULL), n)
  quo_id <- map_lgl(x, is_quosure)
  out[quo_id] <- map(x[quo_id], eval_tidy)

  out[!quo_id] <- map(x[quo_id], eval_tidy, data=data, env = env)
  if(not_vector){
    return(out[[1]])
  }
  out[[1]]
}

