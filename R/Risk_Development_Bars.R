#' Create an Interactive Risk Share Bar-plot (Upside or Downside)
#'
#' This function reads SMA survey data from multiple files and visualizes the composition of
#' upside or downside risk over time as an interactive stacked bar plot.
#'
#' @param file_paths_named_list Named list of Excel file paths.
#' @param type Character string specifying risk type: either "Upside" (default) or "Downside".
#'
#' @return A \code{plotly} interactive bar plot showing risk share distribution over time.
#'
#' @author Ole Paech
#'
#' @examples
#' \dontrun{
#' file_paths <- prepare_file_list(c("May 25", "Jun 25"))
#' risk_share_bars(file_paths, type = "Upside")    #Development of Upside Risks
#' risk_share_bars(file_paths, type = "Downside")  #Development of Downside Risks
#' }
#'
#' @export
risk_share_bars <- function(file_paths_named_list, type = "Upside") {
  importance_map <- c(
    "Absolutely no relevance" = 0,
    "Not so Important" = 1,
    "Moderate" = 2,
    "Important" = 3,
    "Very Important" = 4
  )

  if (!type %in% c("Upside", "Downside")) {
    stop("type must be either 'Upside' or 'Downside'")
  }

  process_file <- function(path, label) {
    df <- readxl::read_excel(path)

    df <- df %>%
      dplyr::mutate(
        Inflation = df[[18]] %>%
          as.character() %>%
          stringr::str_replace_all("%", "") %>%
          stringr::str_replace_all(",", ".") %>%
          as.numeric()
      ) %>%
      dplyr::select(Inflation, 19:28) %>%
      dplyr::mutate(dplyr::across(2:11, ~ importance_map[.])) %>%
      dplyr::mutate(Source = label)

    return(df)
  }

  all_data <- purrr::map2_dfr(file_paths_named_list, names(file_paths_named_list), process_file)

  all_data <- all_data %>%
    dplyr::mutate(Source = factor(Source, levels = names(file_paths_named_list)))

  if (type == "Upside") {
    selected_cols <- 2:6
    prefix <- "Upside"
    title <- "Composition of Upside Risk Over Time"
  } else {
    selected_cols <- 7:11
    prefix <- "Downside"
    title <- "Composition of Downside Risk Over Time"
  }

  summary_data <- all_data %>%
    dplyr::group_by(Source) %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(selected_cols), ~ mean(., na.rm = TRUE), .names = paste0(prefix, "_", "{col}")))

  risk_shares <- summary_data %>%
    tidyr::pivot_longer(-Source, names_to = "Risk", values_to = "Value") %>%
    dplyr::mutate(Risk = stringr::str_remove(Risk, paste0("^", prefix, "_"))) %>%
    dplyr::group_by(Source) %>%
    dplyr::mutate(Share = Value / sum(Value, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(text = paste0("Risk: ", Risk, "\nDate: ", Source, "\nShare: ", round(Share * 100, 1), "%"))

  farben <- c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e")
  risks <- sort(unique(risk_shares$Risk))
  farben_named <- stats::setNames(rep_len(farben, length(risks)), risks)

  p <- ggplot2::ggplot(risk_shares, ggplot2::aes(x = Source, y = Share, fill = Risk, text = text)) +
    ggplot2::geom_col(position = "fill", color = "black", width = 0.7) +
    ggplot2::scale_y_continuous(labels = scales::percent_format()) +
    ggplot2::scale_fill_manual(values = farben_named) +
    ggplot2::labs(
      title = title,
      x = "Survey Date",
      y = paste("Share of Total", type, "Risk"),
      fill = paste(type, "Risk")
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      text = ggplot2::element_text(size = 14),
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5)
    )

  return(plotly::ggplotly(p, tooltip = "text"))
}
