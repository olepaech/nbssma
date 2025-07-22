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
aggregated_risk_factors <- function(df, infl_col = c(16), upside_col = c(17:22), downside_col = c(24:29),
                         xlab = "", ylab = "Average Inflation Expectation", title = "") {
  suppressWarnings({
    inflation_col <- base::names(df)[infl_col]
    risks_1 <- base::gsub("2", "", base::names(df)[upside_col])
    risks_2 <- base::gsub("2", "", base::names(df)[downside_col])

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

    farben <- c("Upside Risks" = "#1c355e", "Downside Risks" = "#0067ab")

    p <- ggplot2::ggplot() +
      ggplot2::geom_rect(data = df_plot[1, ],
                         ggplot2::aes(xmin = 0.7, xmax = 1.3,
                                      ymin = inflation_value,
                                      ymax = inflation_value + Value,
                                      fill = Group),
                         color = "black") +
      ggplot2::geom_rect(data = df_plot[2, ],
                         ggplot2::aes(xmin = 0.7, xmax = 1.3,
                                      ymin = inflation_value - Value,
                                      ymax = inflation_value,
                                      fill = Group),
                         color = "black") +
      ggplot2::geom_point(ggplot2::aes(x = 1, y = inflation_value,
                                       text = base::paste0("Average Inflation Expectation: ", base::round(inflation_value, 2))),
                          color = "#a2a9ad", size = 2) +
      ggplot2::geom_hline(yintercept = inflation_value, linetype = "dashed") +
      ggplot2::scale_fill_manual(values = farben) +
      ggplot2::scale_x_continuous(breaks = 1, labels = "Risks") +
      ggplot2::scale_y_continuous(name = ylab) +
      ggplot2::labs(x = xlab, title = title, fill = "") +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(size = 12, face = "bold"),
        axis.ticks.x = ggplot2::element_blank(),
        legend.position = "right",
        legend.title = ggplot2::element_blank()
      )

    plotly::ggplotly(p, tooltip = "text")
  })
}
