#' Create Interactive Risk Factors Bar Plot
#'
#' This function takes a data frame from SMA survey results and creates an interactive bar plot
#' showing average and weighted upside and downside risk factors alongside inflation expectation.
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
risk_factors <- function(df, infl_col = c(16), upside_col = c(17:22), downside_col = c(24:29), xlab = "", ylab = "Average Inflation Expectation", title = ""){
  suppressWarnings({
    inflation_col <- names(df)[infl_col]
    risks_1 <- names(df)[upside_col]
    risks_2 <- names(df)[downside_col]

    risks_1 <- gsub("2", "", risks_1)
    risks_2 <- gsub("2", "", risks_2)

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

    risk1_means <- colMeans(df_clean %>% dplyr::select(dplyr::starts_with("num_") & dplyr::all_of(paste0("num_", risks_1))), na.rm = TRUE)
    risk2_means <- colMeans(df_clean %>% dplyr::select(dplyr::starts_with("num_") & dplyr::all_of(paste0("num_", risks_2))), na.rm = TRUE)
    inflation_value <- mean(df_clean$inflation_value, na.rm = TRUE)

    df_plot <- data.frame(
      Variable = c(paste0("Upside_", risks_1), paste0("Downside_", risks_2)),
      Value = c(risk1_means, -risk2_means),
      Group = c(rep("Q-V", length(risks_1)), rep("X–AC", length(risks_2)))
    )

    farben <- c("#1C355E", "#CCE1EE", "#74253E", "#00594F", "#D15F27","white",
                "#1C355E", "#c7932c", "#74253E", "#CCE1EE", "#A2A9AD", "white")
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
        x = ifelse(Group == "Q-V", "Upside Risks", "Downside Risks")
      ) %>%
      dplyr::group_by(Group) %>%
      dplyr::mutate(share = round(abs(Value) / sum(abs(Value)) * 100, 1)) %>%
      dplyr::ungroup()

    total_upside <- round(sum(df_plot$Value[df_plot$Group == "Q-V"]), 2)
    total_downside <- round(sum(abs(df_plot$Value[df_plot$Group == "X–AC"])), 2)

    p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = x, ymin = ymin, ymax = ymax, fill = Variable,
                                               text = paste0(Variable, ": ", share, "%"))) +
      ggplot2::geom_rect(ggplot2::aes(xmin = 0.7, xmax = 1.3), color = "black") +
      ggplot2::geom_hline(yintercept = inflation_value, linetype = "dashed", linewidth = 1.1) +
      ggplot2::geom_point(ggplot2::aes(x = 1, y = inflation_value,
                                       text = paste0("Average Inflation Expectation: ", round(inflation_value, 2))),
                          color = "transparent") +
      ggplot2::annotate("text",
                        x = 1,
                        y = max(df_plot$ymax[df_plot$Group == "Q–V"]) + 0.5,
                        label = paste0("Upside Risk"),
                        size = 4, fontface = "bold", hjust = 0.5) +
      ggplot2::annotate("text",
                        x = 1,
                        y = min(df_plot$ymin[df_plot$Group == "X–AC"]) - 2.2,
                        label = paste0("Downside Risk"),
                        size = 4, fontface = "bold", hjust = 0.5) +
      ggplot2::scale_fill_manual(values = farben) +
      ggplot2::scale_y_continuous(name = ylab,
                                  breaks = seq(
                                    floor(min(df_plot$ymin, na.rm = TRUE)),
                                    ceiling(max(df_plot$ymax, na.rm = TRUE)),
                                    by = 0.5
                                    )) +
      ggplot2::theme_minimal() +
      ggplot2::labs(x = xlab, title = title) +
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
