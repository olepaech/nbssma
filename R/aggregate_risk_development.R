#' Create an Interactive Risk Share Bar-plot (Upside or Downside)
#'
#' This function reads SMA survey data from multiple files and visualizes the composition of
#' upside or downside risk over time as an interactive stacked bar plot.
#'
#' @param df A data frame containing survey data.
#' @param infl_col A vector stating in which column of the file the inflation data are.
#' @param upside_col A vector stating in which columns of the file the upside risk data are.
#' @param downside_col A vector stating in which columns of the file the downside risk data are.
#' @param xlab A character string specifying the x-axis label (optional).
#' @param ylab A character string specifying the y-axis label (optional).
#' @param title A character string specifying the title of the graph (optional).
#'
#' @return A \code{plotly} interactive bar plot.
#'
#' @author Ole Paech
#'
#' @examples
#' \dontrun{
#' files <- prepare_file_list(c("May 25", "Jun 25"))
#' aggregate_risk_development(files)
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate across select starts_with arrange group_by ungroup summarise
#' @importFrom ggplot2 ggplot aes geom_rect geom_hline geom_point annotate scale_fill_manual scale_y_continuous theme_minimal labs theme element_text element_blank
#' @importFrom plotly ggplotly
#'
#' @export
aggregate_risk_development <- function(file_paths_named_list,
                                      infl_col = c(16),
                                      upside_col = c(17:22),
                                      downside_col = c(24:29),
                                      xlab = "Survey Date",
                                      ylab = "Share of Total Risk",
                                      title = "Share of Aggregated Upside vs Downside Risk Over Time") {

  importance_map <- c(
    "Absolutely no relevance" = 0,
    "Not so Important" = 1,
    "Moderate" = 2,
    "Important" = 3,
    "Very Important" = 4
  )

  process_file <- function(path, label) {
    df <- readxl::read_excel(path)

    col_names <- c("Inflation", paste0("Upside_", seq_along(upside_col)), paste0("Downside_", seq_along(downside_col)))

    df <- df %>%
      dplyr::mutate(
        Inflation = df[[infl_col]] %>%
          as.character() %>%
          stringr::str_replace_all("%", "") %>%
          stringr::str_replace_all(",", ".") %>%
          as.numeric()
      ) %>%
      dplyr::select(Inflation, dplyr::all_of(upside_col), dplyr::all_of(downside_col)) %>%
      stats::setNames(col_names) %>%
      dplyr::mutate(dplyr::across(starts_with("Upside_"), ~ importance_map[.])) %>%
      dplyr::mutate(dplyr::across(starts_with("Downside_"), ~ importance_map[.])) %>%
      dplyr::mutate(Source = label)

    return(df)
  }

  all_data <- purrr::map2_dfr(file_paths_named_list, names(file_paths_named_list), process_file) %>%
    dplyr::mutate(Source = factor(Source, levels = names(file_paths_named_list)))

  # Summe Upside & Downside pro Survey-Date
  agg_data <- all_data %>%
    dplyr::group_by(Source) %>%
    dplyr::summarise(
      Upside_Sum = sum(dplyr::c_across(starts_with("Upside_")), na.rm = TRUE),
      Downside_Sum = sum(dplyr::c_across(starts_with("Downside_")), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      Total = Upside_Sum + Downside_Sum,
      Upside_Share = Upside_Sum / Total,
      Downside_Share = Downside_Sum / Total
    ) %>%
    dplyr::select(Source, Upside_Share, Downside_Share) %>%
    tidyr::pivot_longer(cols = c("Upside_Share", "Downside_Share"),
                        names_to = "Risk_Type", values_to = "Share") %>%
    dplyr::mutate(
      Risk_Type = dplyr::case_when(
        Risk_Type == "Upside_Share" ~ "Upside Risk",
        Risk_Type == "Downside_Share" ~ "Downside Risk"
      ),
      text = paste0("Date: ", Source, "\nRisk Type: ", Risk_Type, "\nShare: ", round(Share * 100, 1), "%")
    )

  colors <- c("Upside Risk" = "#1c355e", "Downside Risk" = "#0067ab")

  p <- ggplot2::ggplot(agg_data, ggplot2::aes(x = Source, y = Share, fill = Risk_Type, text = text)) +
    ggplot2::geom_col(position = ggplot2::position_stack(reverse = TRUE), width = 0.7, color = NA) +
    ggplot2::scale_y_continuous(labels = scales::percent_format()) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::labs(
      title = title,
      x = xlab,
      y = ylab,
      fill = "Risk Type"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      text = ggplot2::element_text(size = 14),
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      legend.position = "right",
      legend.title = element_blank()
    )

  return(plotly::ggplotly(p, tooltip = "text"))
}
