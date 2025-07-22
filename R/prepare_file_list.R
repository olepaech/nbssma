#' Prepare a named list of file paths
#'
#' Creates a single named list of file paths. This function is makes the creation of the "files_with_labels" for
#' the function "inflation_risk_history" more convenient.
#'
#' @param month_labels A character vector of month labels to load (e.g., c("May 25", "Jun 25")).
#'
#' @examples
#' \dontrun{
#' file_paths <- prepare_file_list(c("May 25", "Jun 25"))
#' }
#'
#'
#' @author Ole Paech
#' @return A named character vector of existing file paths, excluding any missing files.
#' @export
prepare_file_list <- function(month_labels = NULL) {
  month_files <- purrr::set_names(
    purrr::map(month_labels, load_risk_files),
    month_labels
  )

  current_files <- load_participant_files()

  if(length(current_files) > 1){
    current_named <- setNames(current_files, paste0("Current_", seq_along(current_files)))
  } else if(length(current_files) == 1){
    current_named <- setNames(current_files, "Current")
  } else {
    current_named <- NULL
  }

  combined <- c(month_files, current_named)

  combined <- combined[!sapply(combined, is.null)]

  return(combined)
}
