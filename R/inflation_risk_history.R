#' Visualize Historical Inflation Risk Perception
#'
#' This function processes labeled Excel survey files and visualizes
#' the historical development of upside and downside inflation risk
#' in relation to inflation expectations using a stacked bar plot.
#'
#' @param files_with_labels A chronological named list, where each element is the path
#'   to an Excel file and the name is the corresponding survey date label.
#' @param infl_col A vector stating in which column of the file the inflation data are.
#' @param upside_col A vector stating in which columns of the file the upside risk data are.
#' @param downside_col A vector stating in which columns of the file the downside risk data are.
#' @param xlab A character string specifying the x-axis label (optional).
#' @param ylab A character string specifying the y-axis label (optional).
#' @param title A character string specifying the title of the graph (optional).
#'
#'
#' @return An interactive plotly object visualizing the inflation expectation
#'   and risk factor contribution/s over time.
#'
#'
#'
#' @examples
#' \dontrun{
#' files <- prepare_file_list(c("May 25", "Jun 25"))
#' inflation_risk_history(files, infl_col = c(16), upside_col = c(17:21), downside_col = c(23:27))
#' }
#'
#' @author Ole Paech
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate select across bind_rows group_by summarise arrange ungroup filter left_join
#' @importFrom stringr str_replace_all
#' @importFrom readxl read_excel
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot geom_rect aes geom_line geom_point geom_text scale_x_continuous scale_y_continuous scale_fill_manual labs theme_minimal theme element_text
#' @importFrom plotly ggplotly
#' @importFrom stats na.omit
#'
#' @export
inflation_risk_history <- function(files_with_labels, infl_col = c(16), upside_col = c(17:22), downside_col = c(24:29), xlab = "", ylab = "Average Inflation Expectations (in %)",  title = "Development of Inflation Projections and percieved Risks") {
  suppressWarnings({
    suppressMessages({
      importance_map <- c(
        "Absolutely no relevance" = 0,
        "Not so Important" = 0.5,
        "Moderate" = 1.0,
        "Important" = 1.5,
        "Very Important" = 2.0
      )

      process_file <- function(path, label) {
        df <- readxl::read_excel(path)

        df <- dplyr::mutate(df,
                            Inflation = df[[infl_col]] %>%
                              as.character() %>%
                              stringr::str_replace_all("%", "") %>%
                              stringr::str_replace_all(",", ".") %>%
                              as.numeric()
        ) %>%
          dplyr::select(Inflation, upside_col,downside_col) %>%
          dplyr::mutate(dplyr::across(2:13, ~ importance_map[.])) %>%
          dplyr::mutate(Source = label)

        return(df)
      }

      all_data <- dplyr::bind_rows(
        lapply(seq_along(files_with_labels), function(i) {
          process_file(files_with_labels[[i]], names(files_with_labels)[i])
        })
      )

      all_data <- dplyr::mutate(all_data,
                                Source = factor(Source, levels = names(files_with_labels))
      )

      summary_data <- all_data %>%
        dplyr::group_by(Source) %>%
        dplyr::summarise(
          inflation_exp = mean(Inflation, na.rm = TRUE),
          dplyr::across(2:7, ~ mean(., na.rm = TRUE), .names = "Upside_{.col}"),
          dplyr::across(8:13, ~ mean(., na.rm = TRUE), .names = "Downside_{.col}")
        )

      upside <- summary_data %>%
        dplyr::select(Source, dplyr::starts_with("Upside_")) %>%
        tidyr::pivot_longer(-Source, names_to = "Risk", values_to = "Value") %>%
        dplyr::mutate(Type = "Upside")

      downside <- summary_data %>%
        dplyr::select(Source, dplyr::starts_with("Downside_")) %>%
        tidyr::pivot_longer(-Source, names_to = "Risk", values_to = "Value") %>%
        dplyr::mutate(Type = "Downside")

      stack_data <- dplyr::bind_rows(upside, downside)

      stack_data <- dplyr::left_join(
        stack_data,
        dplyr::select(summary_data, Source, inflation_exp),
        by = "Source"
      )

      stack_data <- stack_data %>%
        dplyr::group_by(Source, Type) %>%
        dplyr::arrange(Source, Type, Risk) %>%
        dplyr::mutate(
          Share = Value / sum(Value, na.rm = TRUE),
          Total = sum(Value, na.rm = TRUE),
          cum_share = cumsum(Share),
          ymin = ifelse(Type == "Upside",
                        inflation_exp + Total * (cum_share - Share),
                        inflation_exp - Total * cum_share
          ),
          ymax = ifelse(Type == "Upside",
                        inflation_exp + Total * cum_share,
                        inflation_exp - Total * (cum_share - Share)
          )
        ) %>%
        dplyr::ungroup()

      colors <- c(
        "#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e",
        "#a6761d", "#666666", "#e6ab02", "#1f78b4", "#b2df8a"
      )

      unique_risks <- sort(unique(stack_data$Risk))
      stack_data$Risk <- factor(stack_data$Risk, levels = unique_risks)
      colors_named <- setNames(colors, levels(stack_data$Risk))

      inflation_points <- summary_data %>%
        dplyr::mutate(
          x = as.numeric(factor(Source)),
          text = paste0("Inflation Expectation: ", round(inflation_exp, 2), "%")
        )

      totals_df <- stack_data %>%
        dplyr::group_by(Source, Type) %>%
        dplyr::summarise(
          Total = unique(Total),
          inflation_exp = unique(inflation_exp),
          x = as.numeric(unique(Source))
        ) %>%
        dplyr::mutate(
          y = ifelse(Type == "Upside", inflation_exp + Total + 0.3, inflation_exp - Total - 0.3),
          label = paste0("Total ", Type, ": ", round(Total, 2))
        )

      p <- ggplot2::ggplot() +
        ggplot2::geom_rect(
          data = stack_data,
          ggplot2::aes(
            xmin = as.numeric(factor(Source)) - 0.3,
            xmax = as.numeric(factor(Source)) + 0.3,
            ymin = ymin,
            ymax = ymax,
            fill = Risk,
            text = paste0(Risk, ": ", round(Share * 100, 1), "%")
          ),
          color = "black"
        ) +
        ggplot2::geom_line(
          data = summary_data,
          ggplot2::aes(
            x = as.numeric(factor(Source)),
            y = inflation_exp,
            group = 1
          ),
          color = "black", size = 1.2
        ) +
        ggplot2::geom_point(
          data = inflation_points,
          ggplot2::aes(
            x = x,
            y = inflation_exp,
            text = text
          ),
          color = "black", size = 2
        ) +
        ggplot2::geom_text(
          data = dplyr::filter(totals_df, Type == "Upside"),
          ggplot2::aes(x = x, y = y, label = label),
          size = 4, vjust = 0, fontface = "bold"
        ) +
        ggplot2::geom_text(
          data = dplyr::filter(totals_df, Type == "Downside"),
          ggplot2::aes(x = x, y = y, label = label),
          size = 4, vjust = 1.2, fontface = "bold"
        ) +
        ggplot2::scale_x_continuous(
          breaks = 1:length(files_with_labels),
          labels = names(files_with_labels)
        ) +
        ggplot2::scale_y_continuous(
          breaks = seq(0, ceiling(max(stack_data$ymax, na.rm = TRUE)), by = 2)
        ) +
        ggplot2::scale_fill_manual(values = colors_named) +
        ggplot2::labs(
          y = ylab,
          x = xlab,
          fill = "Risk Factor",
          title = title
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          text = ggplot2::element_text(size = 14),
          plot.title = ggplot2::element_text(face = "bold", hjust = 0.5)
        )

      plotly::ggplotly(p, tooltip = "text")
    })
  })
}
