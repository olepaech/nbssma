#' Create a jittered dotplot of rates by month and category
#'
#' This function takes NBS survey data and creates a jittered dotplot
#' showing individual rate observations by month, colored by a
#' respondent category (profession, experience, or nationality)
#'
#' @param data A data frame containing survey data.
#' @param category A string indicating the grouping category. Must be one of
#'   `"Profession"`, `"Experience"`, or `"Nationality"`.
#'
#' @return A Plotly interactive plot object.
#'
#' @examples
#' \dontrun{
#'   path <- load_participant_files()
#'   data <- readxl::read_excel(path)
#'   dotplot(data, category = "Experience")
#' }
#'
#' @author Ole Paech
#' @export
dotplot <- function(data, category) {
  category_map <- list(
    "Profession" = "What is your profession?",
    "Experience" = "How many years of expertise do you have?",
    "Nationality" = "What is your nationality?"
  )

  if (!(category %in% names(category_map))) {
    stop("Invalid Category. Please choose between 'Profession', 'Experience' or 'Nationality'.")
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

  month_levels <- unique(data_long$Month)[
    order(match(unique(data_long$Month), extract_label(relevant_cols)))
  ]

  data_long <- data_long |>
    dplyr::mutate(Month = factor(Month, levels = month_levels))

  p <- ggplot2::ggplot(data_long, ggplot2::aes(
    x = Month,
    y = Rate,
    color = .data[[category_col]],
    text = paste0(
      category, ": ", .data[[category_col]], "<br>",
      "Expectation: ", Rate
    )
  )) +
    ggplot2::geom_jitter(width = 0.2, height = 0.02, size = 3, alpha = 0.8) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = "", y = "Rate (in %)",
      color = category
    ) +
    ggplot2::theme(
      text = ggplot2::element_text(size = 12),
      plot.title = ggplot2::element_text(hjust = 0.5)
    )

  plotly::ggplotly(p, tooltip = "text")
}
