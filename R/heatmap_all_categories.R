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
#' @param rel_cols A vector stating in which columns of the file the data to visualize are.
#' @param title A character string specifying the title of the graph (optional).
#'
#' @return A `plotly` interactive heatmap object.
#'
#'
#' @examples
#' \dontrun{
#' path <- load_participant_files()
#' data <- readxl::read_excel(path)
#' heatmap_all_categories(data, "Experience", "Profession", rel_cols = c(10,12,14))
#' }
#'
#' @author Ole Paech
#'
#' @importFrom dplyr select all_of mutate across filter group_by summarise
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_replace_all
#' @importFrom ggplot2 ggplot aes geom_tile facet_wrap scale_fill_gradient theme_minimal labs theme element_text
#' @importFrom plotly ggplotly
#' @importFrom stats median
#' @importFrom grid unit
#'
#' @export
heatmap_all_categories <- function(data, category1, category2, rel_cols = c(10,12,14), title = "") {
  category_map <- list(
    "Profession" = "What is your profession? (optional)",
    "Experience" = "How many years of expertise do you have? (optional)",
    "Nationality" = "What is your nationality? (optional)"
  )

  if (!(category1 %in% names(category_map) && category2 %in% names(category_map))) {
    stop("Invalid Category. Please choose between 'Profession', 'Experience' or 'Nationality'.")
  }

  category_col1 <- category_map[[category1]]
  category_col2 <- category_map[[category2]]
  relevant_cols <- names(data)[rel_cols]

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
    dplyr::filter(!is.na(.data[[category_col1]]), .data[[category_col1]] != "") |>
    dplyr::filter(!is.na(.data[[category_col2]]), .data[[category_col2]] != "") |>
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
    "Profession" = c("Data and Statistics", "Economics and Research", "Markets", "Financial Stability and Bank Supervision", "Other"),
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
    ggplot2::scale_fill_gradient(low = "white", high = "#1c355e") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::labs(
      x = category1,
      y = category2,
      title = title,
      fill = "Median"
    ) +
    ggplot2::theme(
      text = ggplot2::element_text(),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      axis.text.y = ggplot2::element_text(),
      axis.title = ggplot2::element_text(),
      plot.title = ggplot2::element_text(hjust = 0.5),
      panel.spacing = grid::unit(2, "lines")
    )

  plotly::ggplotly(p, tooltip = "text") |>
    plotly::layout()
}
