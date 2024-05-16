new_formula2 <- function (lhs = NULL, rhs, env = caller_env(), str2lang =TRUE) {
  if(str2lang){
    if(is_string(lhs)){ lhs <- str2lang(lhs) }
    if(is_string(rhs)){ rhs <- str2lang(rhs) }
  }
  new_formula(lhs = lhs, rhs = rhs, env = env)
}
new_fml <- new_formula2
new_rhs_fml <- function(rhs, env = caller_env(), str2lang =TRUE){
  new_fml(lhs = NULL, rhs = rhs, env = env, str2lang = str2lang)
}
new_lhs_fml <- function(lhs, env = caller_env(), str2lang =TRUE){
  new_fml(lhs = lhs, rhs = NULL, env = env, str2lang = str2lang)
}

new_fmls <- function(lhs, rhs, env = caller_env(), ..., str2lang =TRUE, .named = FALSE){
  n <- vctrs::vec_size_common(lhs, rhs)
  if(length(lhs) == 1){rep(lhs, n)}
  if(length(lhs) == 1){rep(rhs, n)}
  out <- map2(lhs, rhs, new_fml, env = env, str2lang = str2lang)
  if(.named){
    nms0 <- names(out)
    nms <- map_chr(out, f_text)
    if(is.null(nms0)){
      names(out) <- nms
    } else {
      no_nm <- nms0 == ""
      names(out)[no_nm] <- nms[no_nm]
    }
  }
  out
}
new_rhs_fmls <- function(rhs, env = caller_env(), str2lang =TRUE, .named = FALSE){
  out <- map(rhs, new_rhs_fml, env = env, str2lang = str2lang)
  names(out) <- map_chr(out, f_text)
  if(.named){
    nms0 <- names(out)
    nms <- map_chr(out, f_text)
    if(is.null(nms0)){
      names(out) <- nms
    } else {
      no_nm <- nms0 == ""
      names(out)[no_nm] <- nms[no_nm]
    }
  }
  out
}
new_lhs_fmls <- function(lhs, env = caller_env(), str2lang =TRUE, .named = FALSE){
  out <- map(lhs, new_lhs_fml, env = env, str2lang = str2lang)
  names(out) <- map_chr(out, f_text)
  if(.named){
    nms0 <- names(out)
    nms <- map_chr(out, f_text)
    if(is.null(nms0)){
      names(out) <- nms
    } else {
      no_nm <- nms0 == ""
      names(out)[no_nm] <- nms[no_nm]
    }
  }
  out
}
