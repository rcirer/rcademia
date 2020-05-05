#' Reads an excel file exported from S-PRO, containing WIMU data
#'
#' It just pastes a path
#'
#' @param folder_path Path to a directory
#' @param file_name Name of a file within folder_path

#' @return A character string
#' @export
paste_path <- function(folder_path, file_name) {

  if (class(folder_path) != "character") {
    warning(paste("folder_path should be of character class, not",
                  class(folder_path)))
  }

  if (!dir.exists(folder_path)) {

    warning(paste(folder_path, "seems not to exist. Please check it closer"))
  }
  if (class(file_name) != "character") {
    warning(paste("folder_path should be of character class, not",
                  class(file_name)))
  }
  path = file.path(folder_path, file_name)

  # if (!dir.exists(path)) {
  #   warning(paste(path, "seems not to exist. Please check it closer"))
  # }
  return(path)
}




