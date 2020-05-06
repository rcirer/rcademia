#' Read an load the rds files you ask, from dat folder
#'
#' Loads all data objects contained in the project dat folder
#'
#' @param ... Names of the objects you want to load, quoted, without extension.
#' @param folder_path Lets you change the directory where data files are stored
#' @param extension At the moment it only accepts rds files

#' @return
#' @importFrom magrittr %>%
#' @export
load_rds <- function(...,
                     folder_path = "./../dat",
                     extension = "rds") {

  files <- list(...)
  if (length(files) == 0) {
    stop("You should provide at least one filename")
  }
  if (purrr::vec_depth(files) != 2) {
    stop("Depth of files should be 2")
  }
  # shouldn't check somewhere that it is a rds file, or if it exists?
  for (i in seq_along(files)) {
    ith_name = files[i][[1]]
    ith_file = paste(ith_name, extension, sep = ".")
    ith_path = file.path(folder_path, ith_file)
    ith_rds = readr::read_rds(ith_path)
    assign(ith_name, ith_rds, envir=globalenv())
  }
}




