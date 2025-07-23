#' Create a jittered dotplot of rates by month and category
#'
#' This function takes NBS survey data and creates a jittered dotplot
#' showing individual rate observations by month, colored by a
#' respondent category (profession, experience, or nationality)
#'
#' @param data A data frame containing survey data.
#' @param category A string indicating the grouping category. Must be one of
#'   `"Profession"`, `"Experience"`, or `"Nationality"`.
#' @param rel_cols A vector stating in which columns of the file the data to visualize are.
#' @param xlab A character string specifying the x-axis label (optional).
#' @param ylab A character string specifying the y-axis label (optional).
#' @param title A character string specifying the title of the graph (optional).
#'
#' @return A Plotly interactive plot object.
#'
#' @examples
#' \dontrun{
#'   path <- load_participant_files()
#'   data <- readxl::read_excel(path)
#'   dotplot(data, category = "Experience", rel_cols = c(10,12,14))
#' }
#'
#' @author Ole Paech
#'
#' @importFrom dplyr select all_of mutate across filter
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_replace_all
#' @importFrom ggplot2 ggplot aes geom_jitter theme_minimal labs theme element_text element_blank
#' @importFrom plotly ggplotly
#'
#' @export
dotplot <- function(data, category, rel_cols = c(10,12,14), xlab = "", ylab = "Rate (in %)", title = "") {
  category_map <- list(
    "Profession" = "What is your profession?",
    "Experience" = "How many years of expertise do you have?",
    "Nationality" = "What is your nationality?"
  )

  if (!(category %in% names(category_map))) {
    stop("Invalid Category. Please choose between 'Profession', 'Experience' or 'Nationality'.")
  }

  category_col <- category_map[[category]]
  relevant_cols <- names(data)[rel_cols]

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
    dplyr::filter(!is.na(.data[[category_col]]), .data[[category_col]] != "") |>
    dplyr::mutate(Month = extract_label(Question))

  month_levels <- unique(data_long$Month)[
    order(match(unique(data_long$Month), extract_label(relevant_cols)))
  ]

  data_long <- data_long |>
    dplyr::mutate(Month = factor(Month, levels = month_levels))

  my_colors <- c("#1c355e", "#0067ab", "#cce1ee", "#a5835a", "#74253e",
                 "#00594f", "#d15f27", "#c7932c", "#a2a9ad")

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
    ggplot2::scale_color_manual(values = my_colors) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = xlab, y = ylab, title = title,
      color = category
    ) +
    ggplot2::theme(
    text = ggplot2::element_text(size = 12, family = "Arial"),
    plot.title = ggplot2::element_text(hjust = 0.5, family = "Arial"),
    axis.title = ggplot2::element_text(family = "Arial"),
    axis.text = ggplot2::element_text(family = "Arial")
)
  plotly::ggplotly(p, tooltip = "text")
}
