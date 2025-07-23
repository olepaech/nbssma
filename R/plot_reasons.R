#' Display Open-Ended DFR-Expectation Reasons in a Table
#'
#' This function extracts open-ended textual responses associated with DFR expectations
#' from a data frame and displays them in a wide-format table.
#'
#' @param df A data frame containing at least 4 pairs of numeric rate and corresponding text columns.
#' @param rate_col A vector stating in which columns of the file the expected DFR data are.
#' @param text_col A vector stating in which columns of the file the explanations for DFR expectations are.
#'
#'
#' @return A rendered table object showing the textual responses by question.
#'
#' @author Ole Paech
#'
#' @examples
#' \dontrun{
#' path <- load_participant_files()
#' data <- readxl::read_excel(path)
#' table_reasons(data)
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom gt gt tab_header cols_label
#' @importFrom dplyr rename_with
#' @importFrom tidyr pivot_wider
#' @importFrom purrr map2_dfr
#' @importFrom tibble tibble
#'
#' @export
table_reasons <- function(df, rate_col = c(10, 12, 14), text_col = c(11, 13, 15)) {
  rate_cols <- names(df)[rate_col]
  text_cols <- names(df)[text_col]

  text_df <- purrr::map2_dfr(rate_cols, text_cols, ~ {
    label <- extract_label(.x)
    tibble::tibble(
      label = label,
      response = as.character(df[[.y]])
    )
  }) %>%
    dplyr::filter(!is.na(response), response != "") %>%
    dplyr::group_by(label) %>%
    dplyr::mutate(row = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = label, values_from = response, values_fill = "") %>%
    dplyr::rename_with(~ stringr::str_wrap(., width = 20))

  gt::gt(text_df) %>%
    gt::tab_options(
      table.font.names = "Arial",
      table.font.size = 12,
      heading.title.font.weight = "bold",
      heading.subtitle.font.weight = "bold",
      column_labels.font.weight = "bold"  # <-- macht die Spaltentitel fett
    )
}
