#' Grouped Histogram Plot of Expected Rates by Month
#'
#' Creates grouped histograms of expected rates over months with medians highlighted.
#'
#' @param data Data frame containing the survey responses.
#' @param rel_cols A vector stating in which columns of the file the data to visualize are.
#' @param xlab A character string specifying the x-axis label (optional).
#' @param ylab A character string specifying the y-axis label (optional).
#' @param title A character string specifying the title of the graph (optional).
#'
#' @return A plotly interactive histogram plot.
#'
#'
#' @examples
#' \dontrun{
#'   path <- load_participant_files()
#'   data <- readxl::read_excel(path)
#'   grouped_histogram(data)
#' }
#'
#' @author Ole Paech
#'
#' @importFrom dplyr select all_of mutate across filter group_by summarise ungroup
#' @importFrom tidyr pivot_longer complete
#' @importFrom stringr str_replace_all
#' @importFrom ggplot2 ggplot geom_bar geom_text scale_fill_manual labs theme_minimal position_dodge element_text
#' @importFrom plotly ggplotly layout
#' @importFrom stats median
#' @importFrom lubridate dmy today
#' @importFrom tibble tibble
#'
#' @export
grouped_histogram <- function(data, rel_cols = c(10,12,14), xlab = "Expected Rate", ylab = "Percentage (%)", title = "" ) {
  suppressWarnings({
    relevant_cols <- names(data)[rel_cols]

    data_clean <- data |>
      dplyr::select(dplyr::all_of(relevant_cols)) |>
      dplyr::mutate(dplyr::across(
        .cols = dplyr::everything(),
        .fns = ~ .x |>
          stringr::str_replace_all("%", "") |>
          stringr::str_replace_all(",", ".") |>
          as.numeric()
      ))

    data_long <- data_clean |>
      tidyr::pivot_longer(
        cols = dplyr::everything(),
        names_to = "Question",
        values_to = "Rate"
      ) |>
      dplyr::filter(!is.na(Rate)) |>
      dplyr::mutate(Month = extract_label(Question))
    months_unique <- unique(data_long$Month)
    months_unique <- months_unique[months_unique != "Other"]

    long_run_present <- "Long-Run" %in% months_unique
    months_no_longrun <- months_unique[months_unique != "Long-Run"]

    parse_month <- function(m) {
      lubridate::dmy(paste("01", m))
    }

    dates <- vapply(months_no_longrun, parse_month, lubridate::today())
    months_sorted <- months_no_longrun[order(dates)]
    if (long_run_present) {
      months_sorted <- c(months_sorted, "Long-Run")
    }

    data_long$Month <- factor(data_long$Month, levels = months_sorted)

    breaks <- seq(min(data_clean, na.rm = TRUE),
                  max(data_clean, na.rm = TRUE),
                  by = 0.25)
    labels <- paste(head(breaks, -1), "to", tail(breaks, -1))

    data_long$Bin <- cut(data_long$Rate, breaks = breaks, labels = labels,
                         include.lowest = TRUE, right = FALSE)

    Colors <- c("#1c355e", "#0067ab", "#cce1ee", "#a5835a", "#74253e",
                "#00594f", "#d15f27", "#c7932c", "#a2a9ad")
    Colors <- Colors[seq_along(months_sorted)]

    hist_data <- data_long |>
      dplyr::group_by(Month, Bin) |>
      dplyr::summarise(Freq = dplyr::n(), .groups = "drop") |>
      tidyr::complete(Month, Bin, fill = list(Freq = 0)) |>
      dplyr::group_by(Month) |>
      dplyr::mutate(Percent = Freq / sum(Freq) * 100) |>
      dplyr::ungroup()

    median_vals <- vapply(relevant_cols, function(col) {
      stats::median(data_clean[[col]], na.rm = TRUE)
    }, numeric(1))

    median_bins <- cut(median_vals, breaks = breaks, labels = labels,
                       include.lowest = TRUE, right = FALSE)

    bin_midpoints <- (head(breaks, -1) + tail(breaks, -1)) / 2

    median_x <- vapply(median_bins, function(bin) {
      bin_idx <- which(labels == bin)
      bin_midpoints[bin_idx]
    }, numeric(1))

    median_y <- vapply(seq_along(median_bins), function(i) {
      bin <- median_bins[i]
      month <- months_sorted[i]
      val <- hist_data |>
        dplyr::filter(Month == month, Bin == bin) |>
        dplyr::pull(Percent)
      if (length(val) == 0) 0 else val
    }, numeric(1))

    median_df <- tibble::tibble(
      Month = factor(months_sorted, levels = months_sorted),
      Median = median_vals,
      Bin = median_bins,
      x = median_x,
      y = median_y,
      hover_text = paste0("Median ", months_sorted, ": ", round(median_vals, 2))
    )

    p <- ggplot2::ggplot() +
      ggplot2::geom_bar(data = hist_data,
                        mapping = ggplot2::aes(
                          x = Bin, y = Percent, fill = Month,
                          text = paste0(Month, "<br>Bin: ", Bin, "<br>Percent: ", round(Percent, 2), "%")
                        ),
                        stat = "identity",
                        position = ggplot2::position_dodge(width = 0.7),
                        width = 0.7) +
      ggplot2::geom_text(data = median_df,
                         mapping = ggplot2::aes(
                           x = Bin, y = y + max(hist_data$Percent) * 0.03,
                           label = "*", text = hover_text, group = Month
                         ),
                         position = ggplot2::position_dodge(width = 0.7),
                         size = 8,
                         color = Colors,
                         show.legend = FALSE) +
      ggplot2::scale_fill_manual(values = Colors, labels = months_sorted) +
      ggplot2::labs(x = xlab, y = ylab, title = title, fill = "Month") +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        text = ggplot2::element_text(size = 11),
        axis.title = ggplot2::element_text(),
        axis.text = ggplot2::element_text(),
        plot.title = ggplot2::element_text(hjust = 0.5)
      )

    plotly::ggplotly(p, tooltip = "text") |>
      plotly::layout(
        legend = list(title = list(text = "<b> Month </b>"), font = list(family = "Arial")),
        xaxis = list(titlefont = list(family = "Arial"), tickfont = list(family = "Arial")),
        yaxis = list(titlefont = list(family = "Arial"), tickfont = list(family = "Arial")),
        font = list(family = "Arial")
      )
  })
}
