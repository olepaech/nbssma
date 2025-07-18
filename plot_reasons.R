#' Display Open-Ended DFR-Expectation Reasons in a Table
#'
#' This function extracts open-ended textual responses associated with DFR expectations
#' from a data frame and displays them in a wide-format table.
#'
#' @param df A data frame containing at least 4 pairs of numeric rate and corresponding text columns.
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
#' @export
table_reasons <- function(df) {
  rate_cols <- names(df)[c(10, 12, 14, 16)]
  text_cols <- names(df)[c(11, 13, 15, 17)]

  text_df <- purrr::map2_dfr(rate_cols, text_cols, ~ {
    label <- extract_label(.x)
    tibble::tibble(
      label = label,
      response = as.character(df[[.y]])
    )
  }) %>%
    dplyr::filter(!is.na(response), response != "")

  text_df <- text_df %>%
    dplyr::group_by(label) %>%
    dplyr::mutate(row = dplyr::row_number()) %>%
    dplyr::ungroup()

  wide_df <- text_df %>%
    tidyr::pivot_wider(
      names_from = label,
      values_from = response,
      values_fill = list(response = "")
    )

  gridExtra::grid.table(wide_df)
}
