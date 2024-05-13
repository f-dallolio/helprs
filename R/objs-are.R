#' Are objects...?
#'
#' @param .x a vector or a symbolic object (symbol or call).
#' @param .p a function returning a logical vector.
#'
#' @returns The functions `are` and `arenot` return a vector of the same length as `.x`. The functions `is` and `is_not` return a boolean.
#'
#' @name objs-are
#'
#' @rdname objs-are
#' @export
are <- function(.x, .p) {
  map_lgl(.x = .x, .f = .p)
}
#' @rdname objs-are
#' @export
arenot <- function(.x, .p, .all = FALSE) {
  !are(.x, .p = .p, .all = .all)
}
#' @rdname objs-are
#' @export
obj_is <- function(.x, .p) {
  if (is_symbolic(.x)) {
    .x <- list(.x)
  }
  every(.x = .x, .p = .p)
}
#' @rdname objs-are
#' @export
isnot <- function(.x, .p) {
  !obj_is(.x = .x, .p = .p)
}
