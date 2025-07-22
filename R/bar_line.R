#' Create a bar and line plot showing median and IQR over time
#'
#' @param data A data.frame with the survey data.
#' @param rel_cols A vector stating in which columns of the file the data to visualize are.
#' @param title1 A character string specifying the title of the left y-axis (optional).
#' @param title2 A character string specifying the title of the right y-axis (optional)..
#'
#' @return A Plotly object with dual-axis bar and line chart.
#' @examples
#' path <- load_participant_files(subfolder = "Current")
#' data <- readxl::read_excel(path)
#' bar_line(data, rel_cols = c(10,12,14), title1 = "Median (left)", title2 = "Interquartile Range (right)")
#'
#' @author Ole Paech
#' @export
bar_line <- function(data, rel_cols = c(10,12,14), title1 = "Median (left)", title2 = "Interquartile Range (right)") {
  suppressWarnings({
    relevant_cols <- names(data)[rel_cols]

    data_clean <- data |>
      dplyr::select(dplyr::all_of(relevant_cols)) |>
      dplyr::mutate(dplyr::across(
        dplyr::everything(),
        ~ .x |>
          stringr::str_replace_all("%", "") |>
          stringr::str_replace_all(",", ".") |>
          as.numeric()
      ))

    data_long <- data_clean |>
      tidyr::pivot_longer(cols = dplyr::everything(), names_to = "Question", values_to = "Value") |>
      dplyr::filter(!is.na(Value)) |>
      dplyr::mutate(Month = extract_label(Question))  # Achtung: Diese Funktion muss auch im Package definiert werden

    stats <- data_long |>
      dplyr::group_by(Month) |>
      dplyr::summarise(
        Median = stats::median(Value, na.rm = TRUE),
        P25 = stats::quantile(Value, 0.25, na.rm = TRUE),
        P75 = stats::quantile(Value, 0.75, na.rm = TRUE),
        IQR = P75 - P25,
        .groups = "drop"
      ) |>
      dplyr::mutate(Month_date = lubridate::parse_date_time(Month, orders = "b Y")) |>
      dplyr::arrange(Month_date) |>
      dplyr::mutate(Month_numeric = dplyr::row_number()) |>
      dplyr::select(-Month_date)

    x_vals <- stats$Month_numeric
    x_labels <- stats$Month

    plotly::plot_ly() |>
      plotly::add_bars(
        x = x_vals,
        y = ~Median,
        data = stats,
        name = "Median",
        yaxis = "y1",
        marker = list(color = "steelblue"),
        hovertemplate = paste(
          x_labels,
          "<br>Median: %{y:.2f}<extra></extra>"
        )
      ) |>
      plotly::add_trace(
        data = stats,
        x = x_vals,
        y = ~IQR,
        type = "scatter",
        mode = "lines+markers",
        name = "IQR",
        yaxis = "y2",
        line = list(color = "firebrick", width = 3),
        marker = list(size = 6),
        hovertemplate = paste(
          x_labels,
          "<br>IQR: %{y:.2f}<extra></extra>"
        )
      ) |>
      plotly::layout(
        xaxis = list(
          tickmode = 'array',
          tickvals = x_vals,
          ticktext = x_labels,
          title = ''
        ),
        yaxis = list(
          title = title1,
          side = "left",
          showgrid = FALSE,
          zeroline = FALSE
        ),
        yaxis2 = list(
          title = title2,
          overlaying = "y",
          side = "right",
          showgrid = FALSE,
          zeroline = FALSE
        ),
        legend = FALSE
      )
  })
}
