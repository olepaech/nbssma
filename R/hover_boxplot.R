#' Interactive Boxplot for DFR expectations
#'
#' Generates a Plotly boxplot with median and percentile statistics for NBS survey data.
#'
#' @param data A data frame containing survey responses.
#'
#' @return A `plotly` interactive boxplot object.
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
#' @importFrom dplyr select all_of mutate across everything filter group_by summarise ungroup
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_replace_all
#' @importFrom plotly plot_ly add_trace layout
#' @importFrom stats quantile median
#'
#' @export
hover_boxplot <- function(data) {
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

  stats <- data_long %>%
    dplyr::group_by(Month) %>%
    dplyr::summarise(
      Min = min(Rate),
      P25 = stats::quantile(Rate, 0.25),
      Median = stats::median(Rate),
      P75 = stats::quantile(Rate, 0.75),
      Max = max(Rate),
      .groups = "drop"
    )

  plot <- plotly::plot_ly()

  for (i in seq_len(nrow(stats))) {
    month_data <- dplyr::filter(data_long, Month == stats$Month[i])
    hover_text <- paste0(
      "Month: ", stats$Month[i], "<br>",
      "Min: ", round(stats$Min[i], 2), "<br>",
      "P25: ", round(stats$P25[i], 2), "<br>",
      "Median: ", round(stats$Median[i], 2), "<br>",
      "P75: ", round(stats$P75[i], 2), "<br>",
      "Max: ", round(stats$Max[i], 2)
    )

    plot <- plotly::add_trace(
      plot,
      data = month_data,
      y = ~Rate,
      x = stats$Month[i],
      type = "box",
      name = stats$Month[i],
      marker = list(color = "rgba(0,0,0,0.4)"),
      line = list(color = "steelblue"),
      text = hover_text,
      hoverinfo = text,
      boxpoints = FALSE
    )
  }

  category_order <- extract_label(relevant_cols)

  plot <- plotly::layout(
    plot,
    yaxis = list(title = "Rate (%)"),
    xaxis = list(title = "", categoryorder = "array", categoryarray = category_order)
  )

  plot
}
