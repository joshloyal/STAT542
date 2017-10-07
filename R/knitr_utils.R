library(knitr)

read_package_chunks <- function(path = '.') {
  pkg_files <- list.files(path = path, full.names = TRUE)
  invisible(sapply(pkg_files, read_chunk))
}