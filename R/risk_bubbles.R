#' Interactive Bubble Chart of Upside and Downside Risk Perceptions
#'
#' Converts textual survey responses on perceived risks into numeric scores,
#' calculates average importance for each category, and visualizes them
#' as an interactive bubble chart with tooltips.
#'
#' @param data A data frame containing the survey responses.
#' @param upside Column indices or names for upside risks. Default: 17:22
#' @param downside Column indices or names for downside risks. Default: 24:29
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis.
#' @param title Title of the plot.
#'
#' @return An interactive ggiraph bubble plot.
#'
#' @importFrom dplyr mutate across all_of select summarise group_by bind_rows
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_remove
#' @importFrom ggplot2 ggplot aes labs scale_color_manual geom_point theme_minimal theme element_text position_jitter
#' @importFrom ggiraph geom_point_interactive girafe
#'
#' @export
#'
#' @examples
#' \dontrun{
#' path <- load_participant_files()
#' data <- readxl::read_excel(path)
#' risk_bubbles(data)
#' }
risk_bubbles <- function(data, upside = 17:22, downside = 24:29, xlab = "Risk Type",
                         ylab = "Avg. Importance (0 = low, 4 = high)",
                         title = "Upside and Downside Risk Composition") {
  risks_up <- base::names(data)[upside] |>
    stringr::str_remove("\\s*2$")
  risks_down <- base::names(data)[downside] |>
    stringr::str_remove("\\s*2$")

  mapping <- c(
    "Absolutely no relevance" = 0,
    "Not so Important" = 1,
    "Moderate" = 2,
    "Important" = 3,
    "Very Important" = 4
  )

  df_clean <- data |>
    dplyr::mutate(dplyr::across(
      dplyr::all_of(c(risks_up, risks_down)),
      ~ mapping[base::trimws(base::as.character(.))],
      .names = "num_{.col}"
    ))

  summary_up <- df_clean |>
    dplyr::select(dplyr::all_of(paste0("num_", risks_up))) |>
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "category", values_to = "value") |>
    dplyr::group_by(category) |>
    dplyr::summarise(avg_importance = base::mean(value, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(
      category = stringr::str_remove(category, "num_"),
      group = "Upside"
    )

  summary_down <- df_clean |>
    dplyr::select(dplyr::all_of(paste0("num_", risks_down))) |>
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "category", values_to = "value") |>
    dplyr::group_by(category) |>
    dplyr::summarise(avg_importance = base::mean(value, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(
      category = stringr::str_remove(category, "num_"),
      group = "Downside"
    )

  summary_combined <- dplyr::bind_rows(summary_up, summary_down)
  summary_combined$category <- base::factor(
    summary_combined$category,
    levels = base::unique(summary_combined$category)
  )

  my_colors <- c(
    "#1c355e", "#0067ab", "#cce1ee", "#a5835a",
    "#74253e", "#00594f", "#d15f27", "#c7932c"
  )

  plot <- ggplot2::ggplot(summary_combined, ggplot2::aes(
    x = group,
    y = avg_importance,
    color = category,
    tooltip = paste0("Category: ", category, "\nImportance: ", base::round(avg_importance, 2))
  )) +
    ggiraph::geom_point_interactive(size = 6, alpha = 0.8, position = ggplot2::position_jitter(width = 0.2)) +
    ggplot2::scale_color_manual(values = my_colors) +
    ggplot2::labs(
      title = title,
      x = xlab,
      y = ylab,
      color = "Category"
    ) +
    ggplot2::theme_minimal(base_family = "Arial") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 12),
      legend.position = "right"
    )

  ggiraph::girafe(ggobj = plot)
}
