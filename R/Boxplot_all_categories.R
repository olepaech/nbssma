#' Create facetted boxplots grouped by respondent category
#'
#' This function takes NBS survey data and generates boxplots of expectations
#' grouped by a respondent category (profession, experience, or nationality)
#' across different time periods.
#'
#' @param data A data frame containing survey results.
#' @param category A string indicating the grouping category. Must be one of
#'   `"Profession"`, `"Experience"`, or `"Nationality"`.
#'
#' @return A Plotly object containing facetted boxplots by month and category.
#'
#' @examples
#' \dontrun{
#'   path <- load_participant_files()
#'   data <- readxl::read_excel(path)
#'   boxplot_categories(data, category = "Profession")
#' }
#'
#' @author Ole Paech
#'
#' @importFrom dplyr select all_of mutate across filter group_by summarise left_join ungroup
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_replace_all
#' @importFrom stats median quantile
#' @importFrom ggplot2 ggplot aes geom_boxplot facet_wrap labs theme_minimal theme element_blank element_text
#' @importFrom plotly ggplotly
#'
#' @export
boxplot_categories <- function(data, category) {
  category_map <- list(
    "Profession" = "What is your profession?",
    "Experience" = "How many years of expertise do you have?",
    "Nationality" = "What is your nationality?"
  )

  if (!(category %in% names(category_map))) {
    stop("Invalid category. Please choose between 'Profession', 'Experience' or 'Nationality'.")
  }

  category_col <- category_map[[category]]
  relevant_cols <- names(data)[c(10, 12, 14, 16)]

  data_clean <- data |>
    dplyr::select(dplyr::all_of(category_col), dplyr::all_of(relevant_cols)) |>
    dplyr::mutate(dplyr::across(
      dplyr::all_of(relevant_cols),
      ~ .x |>
        stringr::str_replace_all("%", "") |>
        stringr::str_replace_all(",", ".") |>
        as.numeric()
    ))

  data_long <- data_clean |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(relevant_cols),
      names_to = "Question",
      values_to = "Rate"
    ) |>
    dplyr::filter(!is.na(Rate)) |>
    dplyr::mutate(Month = extract_label(Question))

  stats_data <- data_long |>
    dplyr::group_by(.data[[category_col]], Month) |>
    dplyr::summarise(
      Median = stats::median(Rate),
      Min = min(Rate),
      Max = max(Rate),
      P25 = stats::quantile(Rate, 0.25),
      P75 = stats::quantile(Rate, 0.75),
      .groups = "drop"
    )

  data_long <- dplyr::left_join(data_long, stats_data, by = c(category_col, "Month"))

  month_levels <- unique(data_long$Month)[
    order(match(unique(data_long$Month), extract_label(relevant_cols)))
  ]

  data_long <- data_long |>
    dplyr::mutate(Month = factor(Month, levels = month_levels))

  p <- ggplot2::ggplot(
    data_long,
    ggplot2::aes(x = .data[[category_col]], y = Rate, fill = .data[[category_col]])
  ) +
    ggplot2::geom_boxplot(color = "black") +
    ggplot2::facet_wrap(~ Month) +
    ggplot2::labs(x = NULL, y = "Rate (%)", fill = category) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      legend.position = "right"
    )

  plotly::ggplotly(p, tooltip = "text")
}
