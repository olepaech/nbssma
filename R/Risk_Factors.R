#' Create Interactive Risk Factors Bar Plot
#'
#' This function takes a data frame from SMA survey results and creates an interactive bar plot
#' showing average and weighted upside and downside risk factors alongside inflation expectation.
#'
#' @param df A data frame containing survey data. The 18th column should be inflation data,
#'   columns 19-23 upside risk factors, and columns 24-28 downside risk factors.
#'
#' @return A \code{plotly} interactive bar plot.
#'
#' @author Ole Paech
#'
#' @examples
#' \dontrun{
#' path <- load_participant_files()
#' data <- readxl::read_excel(path)
#' risk_factors(data)
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate across select starts_with arrange group_by ungroup summarise
#' @importFrom ggplot2 ggplot aes geom_rect geom_hline geom_point annotate scale_fill_manual scale_y_continuous theme_minimal labs theme element_text element_blank
#' @importFrom plotly ggplotly
#'
#' @export
risk_factors <- function(df){
  suppressWarnings({
    inflation_col <- names(df)[18]
    risks_1 <- names(df)[19:23]
    risks_2 <- names(df)[24:28]

    mapping <- c(
      "Absolutely no relevance" = 0,
      "Not so Important" = 1,
      "Moderate" = 2,
      "Important" = 3,
      "Very Important" = 4
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

    risk1_means <- colMeans(df_clean %>% dplyr::select(dplyr::starts_with("num_") & dplyr::all_of(paste0("num_", risks_1))), na.rm = TRUE)
    risk2_means <- colMeans(df_clean %>% dplyr::select(dplyr::starts_with("num_") & dplyr::all_of(paste0("num_", risks_2))), na.rm = TRUE)
    inflation_value <- mean(df_clean$inflation_value, na.rm = TRUE)

    df_plot <- data.frame(
      Variable = c(paste0("Upside_", risks_1), paste0("Downside_", risks_2)),
      Value = c(risk1_means, -risk2_means),
      Group = c(rep("S–W", length(risks_1)), rep("X–AB", length(risks_2)))
    )

    farben <- c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e",
                "#a6761d", "#666666", "#e6ab02", "#1f78b4", "#b2df8a")
    df_plot$Variable <- factor(df_plot$Variable, levels = c(paste0("Upside_", risks_1), paste0("Downside_", risks_2)))

    df_plot <- df_plot %>%
      dplyr::arrange(Group, -Value) %>%
      dplyr::group_by(Group) %>%
      dplyr::mutate(
        ymin = cumsum(dplyr::lag(Value, default = 0)),
        ymax = ymin + Value
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        ymin = inflation_value + ymin,
        ymax = inflation_value + ymax,
        x = ifelse(Group == "S–W", "Upside Risks", "Downside Risks")
      ) %>%
      dplyr::group_by(Group) %>%
      dplyr::mutate(share = round(abs(Value) / sum(abs(Value)) * 100, 1)) %>%
      dplyr::ungroup()

    total_upside <- round(sum(df_plot$Value[df_plot$Group == "S–W"]), 2)
    total_downside <- round(sum(abs(df_plot$Value[df_plot$Group == "X–AB"])), 2)

    p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = x, ymin = ymin, ymax = ymax, fill = Variable,
                                               text = paste0(Variable, ": ", share, "%"))) +
      ggplot2::geom_rect(ggplot2::aes(xmin = 0.7, xmax = 1.3), color = "black") +
      ggplot2::geom_hline(yintercept = inflation_value, linetype = "dashed", linewidth = 1.1) +
      ggplot2::geom_point(ggplot2::aes(x = 1, y = inflation_value,
                                       text = paste0("Average Inflation Expectation: ", round(inflation_value, 2))),
                          color = "transparent") +
      ggplot2::annotate("text",
                        x = 1,
                        y = max(df_plot$ymax[df_plot$Group == "S–W"]) + 1.5,
                        label = paste0("Upside Risk: ", total_upside),
                        size = 4, fontface = "bold", hjust = 0.5) +
      ggplot2::annotate("text",
                        x = 1,
                        y = min(df_plot$ymin[df_plot$Group == "X–AB"]) - 5.0,
                        label = paste0("Downside Risk: ", total_downside),
                        size = 4, fontface = "bold", hjust = 0.5) +
      ggplot2::scale_fill_manual(values = farben) +
      ggplot2::scale_y_continuous(name = "Average Inflation Expectation") +
      ggplot2::theme_minimal() +
      ggplot2::labs(x = NULL) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_text(size = 10),
        panel.grid.major.x = ggplot2::element_blank(),
        legend.position = "right",
        legend.title = ggplot2::element_blank()
      )

    plotly::ggplotly(p, tooltip = "text")
  })
}
