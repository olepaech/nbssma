#' Load Excel files from a given participant subfolder
#'
#' This function constructs the full path to a directory containing participant Excel files
#' and returns all `.xlsx` file paths found in that directory.
#'
#' @param subfolder A character string specifying the subfolder name within the base path.
#'                  Defaults to `"Current"`.
#'
#' @return A character vector of full file paths to `.xlsx` files.
#'
#' @examples
#' \dontrun{
#'   load_participant_files()              # Loads from "Current"
#'   load_participant_files("July 25")      # Loads from subfolder "July 25"
#' }
#'
#' @author Ole Paech
#' @export
load_participant_files <- function(subfolder = "Current") {
  base_path <- "C:/Users/olepa/OneDrive/Desktop/NBS/Answers"
  full_path <- file.path(base_path, subfolder)
  files <- list.files(full_path, pattern = "\\.xlsx$", full.names = TRUE)
  return(files)
}
