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
  is_ns_sym(x) || is_symbol(x)
}

