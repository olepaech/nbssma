#' Create interactive bar chart of DFR expectations
#'
#' Generates a Plotly bar chart with median, mode, and percentile statistics for NBS survey data.
#'
#' @param data A data frame containing survey responses.
#'
#' @return A `plotly` interactive barplot object.
#'
#' @examples
#' \dontrun{
#' path <- load_participant_files()
#' data <- readxl::read_excel(path)
#' hover_barplot(data)
#' }
#'
#' @author Ole Paech
#' @export
hover_barplot <- function(data){
  relevant_cols <- names(data)[c(10, 12, 14, 16)]
  month_labels <- extract_label(relevant_cols)
  data_numeric <- data %>%
    dplyr::select(dplyr::all_of(relevant_cols)) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ .x %>%
                                  stringr::str_replace_all("%", "") %>%
                                  stringr::str_replace_all(",", ".") %>%
                                  as.numeric()))

  stats <- purrr::map2_dfr(relevant_cols, month_labels, function(colname, label) {
    vals <- data_numeric[[colname]]

    if (all(is.na(vals))) return(tibble::tibble())

    mode_val <- modeest::mfv(vals, na_rm = TRUE)
    mode_val <- if (length(mode_val) > 0) mode_val[1] else NA_real_

    tibble::tibble(
      Month = label,
      Median = stats::median(vals, na.rm = TRUE),
      Mode = mode_val,
      P25 = stats::quantile(vals, 0.25, na.rm = TRUE),
      P75 = stats::quantile(vals, 0.75, na.rm = TRUE)
    )
  })

  fig <- plotly::plot_ly(
    data = stats,
    x = ~Month,
    y = ~Median,
    type = 'bar',
    text = ~paste0(
      "Median: ", round(Median, 2), "<br>",
      "Modus: ", round(Mode, 2), "<br>",
      "25. Percentile: ", round(P25, 2), "<br>",
      "75. Percentile: ", round(P75, 2)
    ),
    hoverinfo = 'text',
    textposition = 'none'
  ) %>%
    plotly::layout(
      yaxis = list(title = "Median Rate (in %)"),
      xaxis = list(title = "", categoryorder = "array", categoryarray = month_labels)
    )

  fig
}
