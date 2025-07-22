#' Create Simplified Interactive Risk Factors Bar Plot
#'
#' This function creates an interactive bar plot showing the combined upside and downside risks
#' alongside the average inflation expectation, without breaking them into individual components.
#'
#' @param df A data frame containing survey data.
#' @param infl_col A vector stating in which column of the file the inflation data are.
#' @param upside_col A vector stating in which columns of the file the upside risk data are.
#' @param downside_col A vector stating in which columns of the file the downside risk data are.
#' @param xlab A character string specifying the x-axis label (optional).
#' @param ylab A character string specifying the y-axis label (optional).
#' @param title A character string specifying the title of the graph (optional).
#'
#' @return A \code{plotly} interactive bar plot.
#' @author Ole Paech
#'
#' @examples
#' \dontrun{
#' path <- load_participant_files()
#' data <- readxl::read_excel(path)
#' risk_factors_composite(data)
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate across select starts_with group_by ungroup summarise all_of
#' @importFrom ggplot2 ggplot aes geom_rect geom_hline geom_point scale_fill_manual scale_y_continuous theme_minimal labs theme element_text element_blank
#' @importFrom plotly ggplotly
#'
#' @export
risk_factors_composite <- function(df, infl_col = c(16), upside_col = c(17:21), downside_col = c(23:27),
                         xlab = "", ylab = "Average Inflation Expectation", title = "") {
  suppressWarnings({
    inflation_col <- names(df)[infl_col]
    risks_1 <- names(df)[upside_col]
    risks_2 <- names(df)[downside_col]

    mapping <- c(
      "Absolutely no relevance" = 0,
      "Not so Important" = 0.5,
      "Moderate" = 1.0,
      "Important" = 1.5,
      "Very Important" = 2.0
    )

    df_clean <- df %>%
      dplyr::mutate(
        inflation_raw = .[[inflation_col]],
        inflation_clean_char = gsub(",", ".", gsub("%", "", inflation_raw)),
        inflation_value = suppressWarnings(as.numeric(trimws(inflation_clean_char)))
      ) %>%
      dplyr::mutate(dplyr::across(
        .cols = dplyr::all_of(c(risks_1, risks_2)),
        .fns = ~ mapping[trimws(as.character(.))],
        .names = "num_{.col}"
      ))

    # Mittelwerte der einzelnen numerischen Risikospalten
    risk1_means <- colMeans(df_clean %>% dplyr::select(dplyr::all_of(paste0("num_", risks_1))), na.rm = TRUE)
    risk2_means <- colMeans(df_clean %>% dplyr::select(dplyr::all_of(paste0("num_", risks_2))), na.rm = TRUE)
    inflation_value <- mean(df_clean$inflation_value, na.rm = TRUE)

    total_upside <- round(sum(risk1_means, na.rm = TRUE), 2)
    total_downside <- round(sum(risk2_means, na.rm = TRUE), 2)

    # Balken beginnen bei Inflationserwartung
    df_plot <- data.frame(
      x = c("Upside Risks", "Downside Risks"),
      ymin = c(inflation_value, inflation_value),
      ymax = c(inflation_value + total_upside, inflation_value - total_downside),
      color = c("Upside", "Downside"),
      text = c(paste0("Upside Risk: ", total_upside),
               paste0("Downside Risk: ", total_downside))
    )

    color_palette <- c("Upside" = "#1b9e77", "Downside" = "#d95f02")

    p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = x, ymin = ymin, ymax = ymax, fill = color, text = text)) +
      ggplot2::geom_rect(ggplot2::aes(xmin = as.numeric(factor(x)) - 0.3,
                                      xmax = as.numeric(factor(x)) + 0.3),
                         color = "black") +
      ggplot2::geom_hline(yintercept = inflation_value, linetype = "dashed", linewidth = 1.1) +
      ggplot2::geom_point(ggplot2::aes(x = 1.5, y = inflation_value,
                                       text = paste0("Average Inflation Expectation: ", round(inflation_value, 2))),
                          color = "transparent") +
      ggplot2::scale_fill_manual(values = color_palette) +
      ggplot2::scale_y_continuous(name = ylab,
                                  breaks = seq(
                                    floor(min(df_plot$ymax, df_plot$ymin, na.rm = TRUE)) - 1,
                                    ceiling(max(df_plot$ymax, df_plot$ymin, na.rm = TRUE)) + 1,
                                    by = 0.5
                                  )) +
      ggplot2::theme_minimal() +
      ggplot2::labs(x = xlab, title = title) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(size = 11),
        axis.text.y = ggplot2::element_text(size = 10),
        panel.grid.major.x = ggplot2::element_blank(),
        legend.position = "none"
      )

    plotly::ggplotly(p, tooltip = "text")
  })
}
