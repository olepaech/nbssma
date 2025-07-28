#' Create Static Risk Bubble Chart
#'
#' This function generates a bubble chart showing the average perceived importance of
#' upside and downside risks based on participant responses.
#'
#' @param data A data frame containing the survey responses with labeled risk columns.
#' @param upside Integer indices indicating the columns representing upside risks.
#' @param downside Integer indices indicating the columns representing downside risks.
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis.
#' @param title Title of the plot.
#'
#' @return A ggplot object showing the static risk bubble chart.
#' @examples
#' \dontrun{
#' path <- nbssma::load_participant_files()
#' data <- readxl::read_excel(path)
#' static_risk_bubble(data)
#' }
#' @author Ole Paech
#' @export
static_risk_bubble <- function(data,
                               upside = 17:22,
                               downside = 24:29,
                               xlab = "Risk Type",
                               ylab = "Avg. Importance (0 = low, 4 = high)",
                               title = "Upside and Downside Risk Composition") {

  risks_up <- stringr::str_remove(names(data)[upside], "\\s*2$")
  risks_down <- stringr::str_remove(names(data)[downside], "\\s*2$")

  risks_up <- paste0("up_", risks_up)
  risks_down <- paste0("down_", risks_down)

  data_renamed <- data
  names(data_renamed)[upside] <- risks_up
  names(data_renamed)[downside] <- risks_down

  mapping <- c(
    "Absolutely no relevance" = 0,
    "Not so Important" = 1,
    "Moderate" = 2,
    "Important" = 3,
    "Very Important" = 4
  )

  df_clean <- data_renamed |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(c(risks_up, risks_down)),
        ~ mapping[trimws(as.character(.))],
        .names = "num_{.col}"
      )
    )

  summary_up <- df_clean |>
    dplyr::select(dplyr::all_of(paste0("num_", risks_up))) |>
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "category", values_to = "value") |>
    dplyr::group_by(category) |>
    dplyr::summarise(avg_importance = mean(value, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(
      category_clean = stringr::str_remove(category, "^num_up_"),
      group = "Upside"
    )

  summary_down <- df_clean |>
    dplyr::select(dplyr::all_of(paste0("num_", risks_down))) |>
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "category", values_to = "value") |>
    dplyr::group_by(category) |>
    dplyr::summarise(avg_importance = mean(value, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(
      category_clean = stringr::str_remove(category, "^num_down_"),
      group = "Downside"
    )

  summary_combined <- dplyr::bind_rows(summary_up, summary_down)

  # Für Farbe und Legende die sauberen Kategorien nutzen
  summary_combined$category_clean <- factor(summary_combined$category_clean, levels = unique(summary_combined$category_clean))

  my_colors <- c(
    "#1c355e", "#0067ab", "#cce1ee", "#a5835a",
    "#74253e", "#00594f", "#d15f27", "#c7932c"
  )

  plot <- ggplot2::ggplot(summary_combined, ggplot2::aes(
    x = group,
    y = avg_importance,
    color = category_clean
  )) +
    ggplot2::geom_point(size = 6, alpha = 0.8, position = ggplot2::position_dodge(width = 0)) +
    ggplot2::scale_color_manual(values = my_colors) +
    ggplot2::labs(
      title = title,
      x = xlab,
      y = ylab,
      color = "Category"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 12),
      legend.position = "right"
    )

  return(plot)
}
