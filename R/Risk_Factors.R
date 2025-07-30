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

    all_risks <- unique(c(risks_1, risks_2))

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
        .cols = dplyr::all_of(c(names(df)[upside_col], names(df)[downside_col])),
        .fns = ~ mapping[trimws(as.character(.))],
        .names = "num_{.col}"
      ))

    risk1_means_raw <- colMeans(df_clean %>% dplyr::select(dplyr::starts_with("num_") & dplyr::all_of(paste0("num_", names(df)[upside_col]))), na.rm = TRUE)
    risk2_means_raw <- colMeans(df_clean %>% dplyr::select(dplyr::starts_with("num_") & dplyr::all_of(paste0("num_", names(df)[downside_col]))), na.rm = TRUE)

    names(risk1_means_raw) <- gsub("^num_", "", names(risk1_means_raw))
    names(risk2_means_raw) <- gsub("^num_", "", names(risk2_means_raw))
    names(risk1_means_raw) <- gsub("2", "", names(risk1_means_raw))
    names(risk2_means_raw) <- gsub("2", "", names(risk2_means_raw))
    risk1_means <- setNames(numeric(length(all_risks)), all_risks)
    risk2_means <- setNames(numeric(length(all_risks)), all_risks)

    risk1_means[names(risk1_means_raw)] <- risk1_means_raw
    risk2_means[names(risk2_means_raw)] <- risk2_means_raw

    inflation_value <- mean(df_clean$inflation_value, na.rm = TRUE)

    df_plot <- data.frame(
      Variable = rep(all_risks, 2),
      Value = c(risk1_means, -risk2_means),
      Group = rep(c("Upside Risks", "Downside Risks"), each = length(all_risks))
    )

    farben <- c("#1c355e", "#0067ab", "#cce1ee", "#d15f27","#A5835A", "#74253e","#00594f", "#A2A9AD", "#c7932c")
    farben_named <- setNames(rep(farben, length.out = length(all_risks)), all_risks)

    df_plot$Variable <- factor(df_plot$Variable, levels = all_risks)

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
        x = Group
      ) %>%
      dplyr::group_by(Group) %>%
      dplyr::mutate(share = round(abs(Value) / sum(abs(Value)) * 100, 1)) %>%
      dplyr::ungroup()

    p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = x, ymin = ymin, ymax = ymax, fill = Variable,
                                               text = paste0(Variable, ": ", share, "%"))) +
      ggplot2::geom_rect(ggplot2::aes(xmin = 0.7, xmax = 1.3), color = "black") +
      ggplot2::geom_hline(yintercept = inflation_value, linetype = "dashed", linewidth = 1.1) +
      ggplot2::geom_point(ggplot2::aes(x = 1, y = inflation_value,
                                       text = paste0("Average Inflation Expectation: ", round(inflation_value, 2))),
                          color = "transparent") +
      ggplot2::annotate("text",
                        x = 1,
                        y = max(df_plot$ymax[df_plot$Group == "Upside Risks"]) + 0.5,
                        label = "Upside Risk",
                        size = 4, hjust = 0.5) +
      ggplot2::annotate("text",
                        x = 1,
                        y = min(df_plot$ymin[df_plot$Group == "Downside Risks"]) - 2.2,
                        label = "Downside Risk",
                        size = 4, hjust = 0.5) +
      ggplot2::scale_fill_manual(values = farben_named) +
      ggplot2::scale_y_continuous(name = ylab,
                                  breaks = seq(
                                    floor(min(df_plot$ymin, na.rm = TRUE)),
                                    ceiling(max(df_plot$ymax, na.rm = TRUE)),
                                    by = 1
                                  )) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::labs(x = xlab, title = title) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(),
        axis.text.y = ggplot2::element_text(),
        plot.title = ggplot2::element_text(hjust = 0.5),
        axis.ticks.x = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_blank(),
        legend.position = "right",
        legend.title = ggplot2::element_blank(),
        text = ggplot2::element_text()
      )

    plotly::ggplotly(p, tooltip = "text")
  })
}
