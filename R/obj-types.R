obj_type <- function(x){
  out <- dplyr::case_match(class(x)[[1]],
                           "name" ~ "sym",
                           "call" ~ "call",
                           "environment" ~ "env",
                           "formula" ~ "fml",
                           "function" ~ "fn",
                           "quosure" ~ "quo",
                           .default = pillar::type_sum(x))
  out
}

obj_fine_type <- function(x){
  x1 <- obj_type(x)
  if(is_ns_call(x)){
    "ns_call"
  } else if (is_ns_sym(x)) {
    "ns_sym"
  } else if (is_environment(x)) {
    env_nm <- attr(x, "name")
    if(is.null(env_nm)){
      env0 <- str_split_i(capture.output(x), ":", 2)
      env0 <- str_squish(env0)
    } else {
      env0 <- str_split_i(env_nm, ":", 1)
    }
    if(env0 == "namespace"){
      "ns"
    } else if (env0 == "package"){
      "pkg"
    } else {
      "env"
    }
  } else {
    obj_type(x)
  }
}

obj_types <- function(x){
  x0 <- as_list0(x)
  map_chr(x0, obj_type)
}

obj_fine_types <- function(x){
  x0 <- as_list0(x)
  map_chr(x0, obj_fine_type)
}
