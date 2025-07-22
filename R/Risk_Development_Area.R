#' Plot Risk Share Area for Upside or Downside Risks
#'
#' This function reads multiple Excel files with survey data and visualizes the composition of
#' upside or downside risk over time as an interactive stacked area plot.
#'
#' @param file_paths_named_list Named list of file paths to Excel files.
#' @param type Character string specifying the risk type: "Upside" (default) or "Downside".
#' @param infl_col A vector stating in which column of the file the inflation data are.
#' @param upside_col A vector stating in which columns of the file the upside risk data are.
#' @param downside_col A vector stating in which columns of the file the downside risk data are.
#' @param xlab A character string specifying the x-axis label (optional).
#'
#' @return A plotly interactive plot showing the share of total risk over time.
#'
#' @author Ole Paech
#'
#' @examples
#' \dontrun{
#' file_paths <- prepare_file_list(c("May 25", "Jun 25"))
#' risk_share_area(file_paths, type = "Upside")    #Development of Upside Risks
#' risk_share_area(file_paths, type = "Downside")  #Development of Downside Risks
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom readxl read_excel
#' @importFrom dplyr mutate select across group_by summarise ungroup
#' @importFrom purrr map2_dfr
#' @importFrom stringr str_replace_all str_remove
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_area geom_line geom_point scale_x_continuous scale_y_continuous scale_fill_manual labs theme_minimal theme element_text position_stack
#' @importFrom scales percent_format
#' @importFrom stats setNames
#' @importFrom plotly ggplotly
#'
#' @export
risk_share_area <- function(file_paths_named_list, type = "Upside", infl_col = c(16), upside_col = c(17:21), downside_col = c(23:27), xlab = "Survey Date") {
  suppressWarnings({
  importance_map <- c(
    "Absolutely no relevance" = 0,
    "Not so Important" = 1,
    "Moderate" = 2,
    "Important" = 3,
    "Very Important" = 4
  )
  if (!type %in% c("Upside", "Downside")) {
    stop("type must be either 'Upside' or 'Downside'")
  }

  process_file <- function(path, label) {
    df <- readxl::read_excel(path)
    df <- df %>%
      dplyr::mutate(
        Inflation = df[[infl_col]] %>%
          as.character() %>%
          stringr::str_replace_all("%", "") %>%
          stringr::str_replace_all(",", ".") %>%
          as.numeric()
      ) %>%
      dplyr::select(Inflation, upside_col, downside_col) %>%
      dplyr::mutate(dplyr::across(2:11, ~ importance_map[.] )) %>%
      dplyr::mutate(Source = label)
    return(df)
  }

  all_data <- purrr::map2_dfr(file_paths_named_list, names(file_paths_named_list), process_file)

  all_data <- all_data %>%
    dplyr::mutate(Source = factor(Source, levels = names(file_paths_named_list)))

  if (type == "Upside") {
    selected_cols <- 2:6
    prefix <- "Upside"
    title <- "Composition of Upside Risk Over Time"
    y_label <- "Share of Total Upside Risk"
    fill_legend <- "Upside Risk"
  } else {
    selected_cols <- 7:11
    prefix <- "Downside"
    title <- "Composition of Downside Risk Over Time"
    y_label <- "Share of Total Downside Risk"
    fill_legend <- "Downside Risk"
  }

  summary_data <- all_data %>%
    dplyr::group_by(Source) %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(selected_cols), ~ mean(., na.rm = TRUE), .names = paste0(prefix, "_{col}")))

  risk_shares <- summary_data %>%
    tidyr::pivot_longer(-Source, names_to = "Risk", values_to = "Value") %>%
    dplyr::mutate(Risk = stringr::str_remove(Risk, paste0("^", prefix, "_"))) %>%
    dplyr::group_by(Source) %>%
    dplyr::mutate(Share = Value / sum(Value, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(text = paste0("Risk: ", Risk, "\nDate: ", Source, "\nShare: ", round(Share * 100, 1), "%"))

  farben <- c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e")
  unique_risks <- sort(unique(risk_shares$Risk))
  farben_named <- stats::setNames(farben[1:length(unique_risks)], unique_risks)
  risk_shares$Risk <- factor(risk_shares$Risk, levels = unique_risks)

  risk_shares <- risk_shares %>%
    dplyr::mutate(x = as.numeric(factor(Source)))

  p <- ggplot2::ggplot(risk_shares, ggplot2::aes(x = x, y = Share, fill = Risk, group = Risk)) +
    ggplot2::geom_area(position = "stack", alpha = 0.9, color = "black", size = 0.2) +
    ggplot2::geom_line(position = ggplot2::position_stack(vjust = 1), size = 1,
                       color = "black", show.legend = FALSE) +
    ggplot2::geom_point(ggplot2::aes(text = text), position = ggplot2::position_stack(vjust = 1),
                        size = 2, color = "black", show.legend = FALSE) +
    ggplot2::scale_x_continuous(breaks = 1:length(levels(risk_shares$Source)), labels = levels(risk_shares$Source)) +
    ggplot2::scale_y_continuous(labels = scales::percent_format()) +
    ggplot2::scale_fill_manual(values = farben_named) +
    ggplot2::labs(
      title = title,
      x = xlab,
      y = y_label,
      fill = fill_legend
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      text = ggplot2::element_text(size = 14),
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5)
    )

  return(plotly::ggplotly(p, tooltip = "text"))
  })
}
