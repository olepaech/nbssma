#' Heatmap of Median Expectations by Two Categories
#'
#' Generates a faceted heatmap comparing the median DFR expectations
#' across two selected respondent categories (profession, experience, or nationality)
#' and over months.
#'
#' @param data A data frame containing survey data.
#' @param category1 A character string specifying the first category (x-Axis).
#' Must be one of `"Profession"`, `"Experience"`, or `"Nationality"`.
#' @param category2 A character string specifying the second category (y-Axis).
#' Must be one of `"Profession"`, `"Experience"`, or `"Nationality"`.
#'
#' @return A `plotly` interactive heatmap object.
#'
#'
#' @examples
#' \dontrun{
#' path <- load_participant_files()
#' data <- readxl::read_excel(path)
#' heatmap_all_categories(data, "Experience", "Profession")
#' }
#'
#' @author Ole Paech
#' @export
heatmap_all_categories <- function(data, category1, category2) {
  category_map <- list(
    "Profession" = "What is your profession?",
    "Experience" = "How many years of expertise do you have?",
    "Nationality" = "What is your nationality?"
  )

  if (!(category1 %in% names(category_map) && category2 %in% names(category_map))) {
    stop("Invalid Category. Please choose between 'Profession', 'Experience' or 'Nationality'.")
  }

  category_col1 <- category_map[[category1]]
  category_col2 <- category_map[[category2]]
  relevant_cols <- names(data)[c(10, 12, 14, 16)]

  data_clean <- data |>
    dplyr::select(dplyr::all_of(category_col1), dplyr::all_of(category_col2), dplyr::all_of(relevant_cols)) |>
    dplyr::mutate(dplyr::across(
      dplyr::all_of(relevant_cols),
      ~ stringr::str_replace_all(., "%", "") |>
        stringr::str_replace_all(",", ".") |>
        as.numeric()
    ))

  data_long <- data_clean |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(relevant_cols),
      names_to = "Question",
      values_to = "Value"
    ) |>
    dplyr::filter(!is.na(Value)) |>
    dplyr::mutate(Month = extract_label(Question))

  month_levels <- unique(data_long$Month)[
    order(match(unique(data_long$Month), extract_label(relevant_cols)))
  ]

  data_long <- data_long |>
    dplyr::mutate(Month = factor(Month, levels = month_levels)) |>
    dplyr::group_by(.data[[category_col1]], .data[[category_col2]], Month) |>
    dplyr::summarise(
      Median_Expectation = stats::median(Value),
      Mean_Expectation = mean(Value),
      .groups = "drop"
    )

  levels_map <- list(
    "Experience" = c("0 - 5 years", "5 - 15 years", "over 15 years"),
    "Profession" = c("Data and Statistics", "Economics and Research", "Markets", "Financial Stability and Bank Supervision"),
    "Nationality" = c("Slovak", "Non-Slovak")
  )

  data_long[[category_col1]] <- factor(
    data_long[[category_col1]],
    levels = levels_map[[category1]]
  )

  p <- ggplot2::ggplot(data_long, ggplot2::aes(
    x = .data[[category_col1]],
    y = .data[[category_col2]],
    fill = Median_Expectation,
    text = paste0(
      category2, ": ", .data[[category_col2]], "<br>",
      category1, ": ", .data[[category_col1]], "<br>",
      "Median: ", round(Median_Expectation, 2), "<br>",
      "Mean: ", round(Mean_Expectation, 2)
    )
  )) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::facet_wrap(~Month, ncol = 2) +
    ggplot2::scale_fill_gradient(low = "white", high = "darkred") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::labs(
      x = category1,
      y = category2,
      fill = "Median"
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      panel.spacing = grid::unit(2, "lines")
    )

  plotly::ggplotly(p, tooltip = "text")
}
