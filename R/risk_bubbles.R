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
risk_bubble <- function(data,
                        upside = 17:22,
                        downside = 24:29,
                        xlab = "Risk Type",
                        ylab = "Avg. Importance (0 = low, 2 = high)",
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
    "Not so Important" = 0.5,
    "Moderate" = 1.0,
    "Important" = 1.5,
    "Very Important" = 2.0
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

  summary_combined$tooltip <- paste0(
    "Category: ", summary_combined$category_clean, "<br>",
    "Group: ", summary_combined$group, "<br>",
    "Avg. Importance: ", round(summary_combined$avg_importance, 2)
  )

  my_colors <- c(
    "#1c355e", "#0067ab", "#cce1ee", "#a5835a",
    "#74253e", "#00594f", "#d15f27", "#c7932c"
  )

  plot <- ggplot2::ggplot(summary_combined, ggplot2::aes(
    x = group,
    y = avg_importance,
    color = category_clean,
    tooltip = tooltip,
    data_id = category_clean
  )) +
    ggiraph::geom_point_interactive(size = 6, alpha = 0.85, position = ggplot2::position_dodge(width = 0)) +
    ggplot2::scale_color_manual(values = my_colors) +
    ggplot2::labs(
      title = title,
      x = xlab,
      y = ylab,
      color = "Category"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 11),
      plot.title = ggplot2::element_text(hjust = 0.5),
      legend.position = "right"
    )

  ggiraph::girafe(ggobj = plot)
}
