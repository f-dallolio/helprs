#' @export
standalone_move <- function() {
  pkg_dir <- here::here()

  r_dir <- fs::path(pkg_dir, "R")

  stdln_dir <- fs::path(pkg_dir, "standalone_files")

  fs::dir_create(stdln_dir)

  usethis::use_build_ignore(stdln_dir)

  stdln_paths <- fs::dir_ls(r_dir, regexp = "-standalone-")

  new_paths <- str_replace_all(stdln_paths, r_dir, stdln_dir)

  walk2(stdln_paths, new_paths, fs::file_copy, overwrite = TRUE)

  walk(stdln_paths, fs::file_delete)
}

#' @export
standalone_rewrite <- function(path, nm) {
  x <- str_flatten(readLines(x), collapse = "\n")

  sprintf("\n# %s -----\n#\n%s", nm, x)
}

#' @export
standalone_append <- function() {
  stdln_dir <- fs::path(pkg_dir, "standalone_files")

  stdln_paths <- as.character(fs::dir_ls(stdln_dir))

  names(stdln_paths) <- str_remove_all(
    fs::path_ext_remove(fs::path_file(stdln_paths)),
    "import-standalone-"
  )

  out <- imap_chr(stdln_paths, standalone_rewrite)

  cat(out, sep = "\n#\n", file = "R/export_standalones.R")

  styler::style_file("R/export_standalones.R")
}
