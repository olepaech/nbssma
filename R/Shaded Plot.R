#' Create an Interactive Shaded Plot of Survey Rates Over Time
#'
#' This function takes a data frame containing SMA survey responses with expected DFR rates over months,
#' cleans and reshapes the data, then creates an interactive plotly plot showing median values
#' along with the interquartile range as a shaded area.
#'
#' @param data A data frame with survey response data.
#'
#' @return A \code{plotly} interactive scatter plot with shaded interquartile range and median lines.
#'
#' @author Ole Paech
#'
#' @examples
#' \dontrun{
#' path <- load_participant_files()
#' data <- readxl::read_excel(path)
#' shaded_plot(data)
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select mutate across everything filter group_by summarise arrange row_number select
#' @importFrom stringr str_replace_all
#' @importFrom tidyr pivot_longer
#' @importFrom stats median quantile
#' @importFrom lubridate parse_date_time
#' @importFrom plotly plot_ly add_trace layout
#'
#' @export
shaded_plot <- function(data) {
  suppressWarnings({
    relevant_cols <- names(data)[c(10,12,14,16)]

    data_clean <- data %>%
      dplyr::select(dplyr::all_of(relevant_cols)) %>%
      dplyr::mutate(dplyr::across(
        .cols = dplyr::everything(),
        .fns = ~ .x %>%
          stringr::str_replace_all("%", "") %>%
          stringr::str_replace_all(",", ".") %>%
          as.numeric()
      ))

    data_long <- data_clean %>%
      tidyr::pivot_longer(cols = dplyr::everything(), names_to = "Question", values_to = "Rate") %>%
      dplyr::filter(!is.na(Rate)) %>%
      dplyr::mutate(Month = extract_label(Question))

    summary_stats <- data_long %>%
      dplyr::group_by(Month) %>%
      dplyr::summarise(
        Median = stats::median(Rate, na.rm = TRUE),
        P25 = stats::quantile(Rate, 0.25, na.rm = TRUE),
        P75 = stats::quantile(Rate, 0.75, na.rm = TRUE)
      ) %>%
      dplyr::mutate(Month_date = lubridate::parse_date_time(Month, orders = "b Y")) %>%
      dplyr::arrange(Month_date) %>%
      dplyr::mutate(Month_numeric = dplyr::row_number()) %>%
      dplyr::select(-Month_date)

    x_vals <- summary_stats$Month_numeric
    x_labels <- summary_stats$Month

    plotly::plot_ly() %>%
      plotly::add_trace(
        x = c(x_vals, rev(x_vals)),
        y = c(summary_stats$P25, rev(summary_stats$P75)),
        type = 'scatter',
        mode = 'none',
        fill = 'toself',
        fillcolor = 'rgba(173, 216, 230, 0.4)',
        hoverinfo = 'skip',
        name = 'Interquartile Range'
      ) %>%
      plotly::add_trace(
        x = x_vals,
        y = summary_stats$Median,
        type = 'scatter',
        mode = 'markers',
        marker = list(opacity = 0),
        hoverinfo = 'text',
        text = paste0(
          x_labels,
          "<br>P25: ", round(summary_stats$P25, 2),
          "<br>P75: ", round(summary_stats$P75, 2)
        ),
        showlegend = FALSE
      ) %>%
      plotly::add_trace(
        x = x_vals,
        y = summary_stats$Median,
        type = 'scatter',
        mode = 'lines+markers',
        line = list(color = 'blue', width = 2),
        marker = list(color = 'blue', size = 8),
        hoverinfo = 'text',
        text = paste0("Median: ", round(summary_stats$Median, 2)),
        name = 'Median'
      ) %>%
      plotly::layout(
        xaxis = list(
          tickmode = 'array',
          tickvals = x_vals,
          ticktext = x_labels,
          title = ''
        ),
        yaxis = list(title = 'Rate (in %)'),
        hovermode = 'x unified',
        showlegend = FALSE
      )
  })
}
