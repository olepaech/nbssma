#' Grouped Bar Plot by Month with Summary Statistics
#'
#' This function takes NBS survey data and creates a grouped bar plot
#' showing Median, Mean and Mode of survey values by a month, colored by
#' a respondent category (profession, experience, or nationality).
#'
#' @param data A data frame containing the survey data.
#' @param category A character string specifying the category to group by.
#'   Must be one of `"Profession"`, `"Experience"`, or `"Nationality"`.
#'
#' @return A Plotly bar plot object.
#'
#'
#' @examples
#' \dontrun{
#'   path <- load_participant_files()
#'   data <- readxl::read_excel(path)
#'   group_bar_month(data, category = "Profession")
#' }
#'
#' @author Ole Paech
#' @export
group_bar_month <- function(data, category) {
  suppressWarnings({
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
        values_to = "Value"
      ) |>
      dplyr::filter(!is.na(Value)) |>
      dplyr::mutate(Month = extract_label(Question))

    month_levels <- unique(data_long$Month)[
      order(match(unique(data_long$Month), extract_label(relevant_cols)))
    ]

    data_long <- data_long |>
      dplyr::mutate(Month = factor(Month, levels = month_levels))

    stats <- data_long |>
      dplyr::group_by(.data[[category_col]], Month) |>
      dplyr::summarise(
        Median = stats::median(Value, na.rm = TRUE),
        P25 = stats::quantile(Value, 0.25, na.rm = TRUE),
        P75 = stats::quantile(Value, 0.75, na.rm = TRUE),
        Mean = mean(Value, na.rm = TRUE),
        Mode = modeest::mfv(Value)[1],
        .groups = "drop"
      )

    plot <- plotly::plot_ly(
      data = stats,
      x = ~Month,
      y = ~Median,
      color = as.formula(paste0("~`", category_col, "`")),
      type = "bar",
      text = ~paste0(
        Month, "<br>",
        "Median: ", round(Median, 2), "<br>",
        "Mean: ", round(Mean, 2), "<br>",
        "Mode: ", round(Mode, 2), "<br>",
        "P25: ", round(P25, 2), "<br>",
        "P75: ", round(P75, 2)
      ),
      hoverinfo = "text"
    )

    plot <- plotly::layout(
      plot,
      xaxis = list(title = category),
      yaxis = list(title = "Median Rate"),
      barmode = "group"
    )

    plot
  })
}
