#' Interactive violin-plot for DFR expectations
#'
#' Generates a Plotly violin-plot with median and percentile statistics for NBS survey data.
#'
#' @param data A data frame containing survey responses.
#'
#' @return A `plotly` interactive violin-plot object.
#'
#' @examples
#' \dontrun{
#' path <- load_participant_files()
#' data <- readxl::read_excel(path)
#' hover_boxplot(data)
#' }
#'
#' @author Ole Paech
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select all_of mutate across everything filter group_by summarise left_join
#' @importFrom stringr str_replace_all
#' @importFrom tidyr pivot_longer
#' @importFrom stats quantile median
#' @importFrom plotly plot_ly add_trace layout
#' @export
hover_violin <- function(data) {
  relevant_cols <- names(data)[c(10, 12, 14, 16)]

  data_clean <- data %>%
    dplyr::select(dplyr::all_of(relevant_cols)) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ .x %>%
                                  stringr::str_replace_all("%", "") %>%
                                  stringr::str_replace_all(",", ".") %>%
                                  as.numeric()))

  data_long <- data_clean %>%
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "Question", values_to = "Rate") %>%
    dplyr::filter(!is.na(Rate)) %>%
    dplyr::mutate(Month = extract_label(Question))

  stats_table <- data_long %>%
    dplyr::group_by(Month) %>%
    dplyr::summarise(
      Min = min(Rate),
      P25 = stats::quantile(Rate, 0.25),
      Median = stats::median(Rate),
      P75 = stats::quantile(Rate, 0.75),
      Max = max(Rate),
      .groups = "drop"
    )

  data_long <- dplyr::left_join(data_long, stats_table, by = "Month")

  plot <- plotly::plot_ly()

  months <- unique(data_long$Month)

  for (m in months) {
    month_data <- dplyr::filter(data_long, Month == m)

    hover_texts <- paste0(
      "Rate: ", round(month_data$Rate, 2), "<br>",
      "Min: ", round(month_data$Min, 2), "<br>",
      "P25: ", round(month_data$P25, 2), "<br>",
      "Median: ", round(month_data$Median, 2), "<br>",
      "P75: ", round(month_data$P75, 2), "<br>",
      "Max: ", round(month_data$Max, 2)
    )

    plot <- plotly::add_trace(
      plot,
      data = month_data,
      type = "violin",
      y = ~Rate,
      x = ~Month,
      name = m,
      box = list(visible = TRUE),
      meanline = list(visible = TRUE),
      opacity = 0.6,
      line = list(color = "black"),
      points = "all",
      text = hover_texts,
      hoverinfo = "text"
    )
  }

  category_order <- extract_label(relevant_cols)

  plot <- plotly::layout(
    plot,
    yaxis = list(title = "Rate"),
    xaxis = list(title = "", categoryorder = "array", categoryarray = category_order)
  )

  plot
}
