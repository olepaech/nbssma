#' Load Risk File by Month Label
#'
#' Loads an Excel file from the Archive folder based on a month label (e.g., "Jun 25").
#' Returns the full file path if the file exists, or `NULL` otherwise.
#'
#' @param month_label A string indicating the month and year of the file, e.g. `"Jun 25"`.
#'
#' @return A character string with the full file path, or `NULL` if the file is not found.
#'
#' #' @examples
#' \dontrun{
#' load_risk_files("Jun 25")
#' }
#'
#' @author Ole Paech
#' @export

load_risk_files <- function(month_label) {
  base_path <- "O:/OMP/a_IR_survey/Archive"
  file_name <- base::paste0("Answers ", month_label, ".xlsx")
  full_path <- base::file.path(base_path, file_name)

  if (!base::file.exists(full_path)) {
    base::warning(base::paste("File not found:", full_path))
    return(NULL)
  }

  return(full_path)
}
