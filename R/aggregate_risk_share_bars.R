#' Create an Interactive Bar Plot of Aggregated Upside vs Downside Risk Over Time
#'
#' This function reads SMA survey data from multiple files and visualizes the relative shares
#' of aggregated upside and downside risks over time in a 100% stacked bar chart.
#'
#' @param file_paths_named_list Named list of Excel file paths.
#' @param infl_col A vector indicating the column index of the inflation data.
#' @param upside_col A vector indicating the column indices of the upside risk assessments.
#' @param downside_col A vector indicating the column indices of the downside risk assessments.
#' @param xlab A character string specifying the x-axis label (optional).
#' @param ylab A character string specifying the y-axis label (optional).
#'
#' @return A \code{plotly} interactive bar plot showing the relative share of upside and downside risks over time.
#'
#' @author Ole Paech
#'
#' @examples
#' \dontrun{
#' file_paths <- prepare_file_list(c("May 25", "Jun 25"))
#' aggregate_risk_share_bars(file_paths)
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom readxl read_excel
#' @importFrom dplyr mutate select across group_by summarise ungroup
#' @importFrom purrr map2_dfr
#' @importFrom stringr str_replace_all
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_col scale_y_continuous scale_fill_manual labs theme_minimal theme element_text
#' @importFrom scales percent_format
#' @importFrom plotly ggplotly
#'
#' @export
aggregate_risk_share_bars<- function(file_paths_named_list,
                                              infl_col = c(16),
                                              upside_col = c(17:21),
                                              downside_col = c(23:27),
                                              xlab = "Survey Date",
                                              ylab = "Share of Total Risk") {

  importance_map <- c(
    "Absolutely no relevance" = 0,
    "Not so Important" = 1,
    "Moderate" = 2,
    "Important" = 3,
    "Very Important" = 4
  )

  process_file <- function(path, label) {
    df <- readxl::read_excel(path)

    df <- df %>%
      dplyr::mutate(
        Inflation = df[[infl_col]] %>%
          as.character() %>%
          stringr::str_replace_all("%", "") %>%
          stringr::str_replace_all(",", ".") %>%
          as.numeric()
      ) %>%
      dplyr::select(Inflation, upside_col, downside_col) %>%
      dplyr::mutate(dplyr::across(2:11, ~ importance_map[.])) %>%
      dplyr::mutate(Source = label)

    return(df)
  }

  all_data <- purrr::map2_dfr(file_paths_named_list, names(file_paths_named_list), process_file)
  all_data <- all_data %>%
    dplyr::mutate(Source = factor(Source, levels = names(file_paths_named_list)))

  summary_data <- all_data %>%
    dplyr::group_by(Source) %>%
    dplyr::summarise(
      Upside = mean(rowMeans(dplyr::across(2:6), na.rm = TRUE), na.rm = TRUE),
      Downside = mean(rowMeans(dplyr::across(7:11), na.rm = TRUE), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      Total = Upside + Downside,
      Upside_Share = Upside / Total,
      Downside_Share = Downside / Total
    ) %>%
    dplyr::select(Source, Downside_Share, Upside_Share) %>%  # Downside first = unten im Stack
    tidyr::pivot_longer(-Source, names_to = "Risk", values_to = "Share") %>%
    dplyr::mutate(
      Risk = dplyr::recode(Risk,
                           "Upside_Share" = "Upside Risk",
                           "Downside_Share" = "Downside Risk"),
      text = paste0("Risk Type: ", Risk, "\nDate: ", Source, "\nShare: ", round(Share * 100, 1), "%")
    )

  summary_data <- summary_data %>%
    dplyr::mutate(
      Risk = factor(Risk, levels = c("Downside Risk", "Upside Risk"))
    ) %>%
    dplyr::arrange(Source, Risk)

  farben <- c("Downside Risk" = "#d95f02", "Upside Risk" = "#1b9e77")

  p <- ggplot2::ggplot(summary_data, ggplot2::aes(x = Source, y = Share, fill = Risk, text = text)) +
    ggplot2::geom_col(position = ggplot2::position_stack(reverse = TRUE), width = 0.7, color = NA) +
    ggplot2::scale_y_continuous(labels = scales::percent_format()) +
    ggplot2::scale_fill_manual(values = farben) +
    ggplot2::labs(
      title = "Stacked Aggregated Upside and Downside Risk Over Time",
      x = xlab,
      y = ylab,
      fill = "Risk Type"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      text = ggplot2::element_text(size = 14),
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5)
    )

  return(plotly::ggplotly(p, tooltip = "text"))
}
