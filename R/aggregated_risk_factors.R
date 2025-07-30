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
#' aggregated_risk_factors(data)
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate across select starts_with arrange group_by ungroup summarise
#' @importFrom ggplot2 ggplot aes geom_rect geom_hline geom_point annotate scale_fill_manual scale_y_continuous theme_minimal labs theme element_text element_blank
#' @importFrom plotly ggplotly
#'
#' @export
aggregated_risk_factors <- function(df, infl_col = c(16), upside_col = c(17:22), downside_col = c(24:29),
                                    xlab = "", ylab = "Average Inflation Expectation", title = "") {
  suppressWarnings({
    inflation_col <- base::names(df)[infl_col]

    risks_1 <- names(df)[upside_col]
    risks_2 <- names(df)[downside_col]
    risks_1 <- gsub("2", "", risks_1)
    risks_2 <- gsub("2", "", risks_2)
    risks_1 <- paste0("up_", risks_1)
    risks_2 <- paste0("down_", risks_2)

    mapping <- c(
      "Absolutely no relevance" = 0,
      "Not so Important" = 0.5,
      "Moderate" = 1.0,
      "Important" = 1.5,
      "Very Important" = 2.0
    )

    data_renamed <- df
    names(data_renamed)[upside_col] <- risks_1
    names(data_renamed)[downside_col] <- risks_2

    df_clean <- data_renamed %>%
      dplyr::mutate(
        inflation_raw = .[[inflation_col]],
        inflation_clean_char = base::gsub(",", ".", base::gsub("%", "", inflation_raw)),
        inflation_value = base::suppressWarnings(base::as.numeric(base::trimws(inflation_clean_char)))
      ) %>%
      dplyr::mutate(dplyr::across(
        .cols = dplyr::all_of(c(risks_1, risks_2)),
        .fns = ~ mapping[base::trimws(base::as.character(.))],
        .names = "num_{.col}"
      ))

    risk1_mean <- base::mean(base::unlist(df_clean %>% dplyr::select(dplyr::all_of(base::paste0("num_", risks_1)))), na.rm = TRUE)
    risk2_mean <- base::mean(base::unlist(df_clean %>% dplyr::select(dplyr::all_of(base::paste0("num_", risks_2)))), na.rm = TRUE)
    inflation_value <- base::mean(df_clean$inflation_value, na.rm = TRUE)

    df_plot <- base::data.frame(
      Group = c("Upside Risks", "Downside Risks"),
      Value = c(risk1_mean, risk2_mean),
      inflation_value = inflation_value
    )
    max_abs_risk <- max(abs(df_plot$Value))

    sec_breaks <- pretty(c(-max_abs_risk, max_abs_risk))
    sec_breaks_scaled <- sec_breaks / max_abs_risk
    sec_breaks_scaled <- sec_breaks_scaled[sec_breaks_scaled >= -1 & sec_breaks_scaled <= 1]
    gridlines_y <- inflation_value + sec_breaks_scaled * max_abs_risk

    farben <- c("Upside Risks" = "#1c355e", "Downside Risks" = "#0067ab")

    p <- ggplot2::ggplot() +
      ggplot2::geom_hline(yintercept = gridlines_y, color = "#a2a9ad", size = 0.3) +

      ggplot2::geom_rect(data = df_plot[1, ],
                         ggplot2::aes(
                           xmin = 0.7, xmax = 1.3,
                           ymin = inflation_value,
                           ymax = inflation_value + Value,
                           fill = Group,
                           text = paste0(Group, ": ", round(Value, 2))
                         ),
                         color = "black") +

      ggplot2::geom_rect(data = df_plot[2, ],
                         ggplot2::aes(
                           xmin = 0.7, xmax = 1.3,
                           ymin = inflation_value - Value,
                           ymax = inflation_value,
                           fill = Group,
                           text = paste0(Group, ": ", round(Value, 2))
                         ),
                         color = "black") +

      ggplot2::geom_point(
        ggplot2::aes(
          x = 1, y = inflation_value,
          text = paste0("Average Inflation Expectation: ", round(inflation_value, 2))
        ),
        color = "#a2a9ad", size = 2
      ) +

      ggplot2::geom_hline(yintercept = inflation_value, linetype = "dashed", color = "#a2a9ad", size = 1) +
      ggplot2::scale_fill_manual(values = farben) +
      ggplot2::scale_x_continuous(breaks = 1, labels = "Risks") +
      ggplot2::scale_y_continuous(
        name = ylab,
        sec.axis = ggplot2::sec_axis(
          trans = ~ (. - inflation_value) / max_abs_risk,
          name = "Risk Weight",
          breaks = sec_breaks_scaled,
          labels = scales::number_format(accuracy = 0.1)(sec_breaks_scaled)
        )) +
      ggplot2::labs(x = xlab, title = title, fill = "") +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(size = 11),
        plot.title = ggplot2::element_text(hjust = 0.5),
        axis.ticks.x = ggplot2::element_blank(),
        legend.position = "right",
        legend.title = ggplot2::element_blank()
      )

    plotly::ggplotly(p, tooltip = "text")
  })
}
