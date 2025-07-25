#' Create interactive bar chart of DFR expectations
#'
#' Generates a Plotly bar chart with median, mode, and percentile statistics for NBS survey data.
#'
#' @param data A data frame containing survey responses.
#' @param rel_cols A vector stating in which columns of the file the data to visualize are.
#' @param xlab A character string specifying the x-axis label (optional).
#' @param ylab A character string specifying the y-axis label (optional).
#' @param title A character string specifying the title of the graph (optional).
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
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select all_of mutate across everything
#' @importFrom stringr str_replace_all
#' @importFrom purrr map2_dfr
#' @importFrom tibble tibble
#' @importFrom stats median quantile
#' @importFrom modeest mfv
#' @importFrom plotly plot_ly layout
#'
#' @export
hover_barplot <- function(data, rel_cols = c(10,12,14), xlab = "", ylab =  "Median Rate (in %)", title = ""){
  relevant_cols <- names(data)[rel_cols]
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
    marker = list(color = "#1c355e"),
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
      yaxis = list(title = ylab, tickfont = list(family = "Arial"), titlefont = list(family = "Arial")),
      xaxis = list(title = xlab, categoryorder = "array", categoryarray = month_labels, tickfont = list(family = "Arial"), titlefont = list(family = "Arial")),
      title = list(text = title, font = list(family = "Arial")),
      font = list(family = "Arial")
    )

  fig
}
