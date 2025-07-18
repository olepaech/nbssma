#' 3D Visualization of Median Inflation Expectations
#'
#' This function creates interactive 3D scatter plots of median and mean inflation expectations
#' by profession, experience level, and nationality across different months.
#'
#' The function is intended to process SMA survey data in wide format and create
#' one 3D plot per observed month.
#'
#' @param data A data frame with variables including profession, experience, nationality,
#'   and inflation expectation columns.
#'
#' @return A list of interactive plotly 3D scatter plots, one for each month.
#'
#' @author Ole Paech
#'
#' @examples
#' \dontrun{
#' path <- load_participant_files()
#' data <- readxl::read_excel(path)
#' plots <- plot_3d(data)
#' plots[[1]]  # Show first months plot
#' plots[[4]]  # Show Long-Run Plot
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select mutate across filter group_by summarise rename arrange ungroup
#' @importFrom tidyselect all_of
#' @importFrom stringr str_replace_all
#' @importFrom tidyr pivot_longer
#' @importFrom plotly plot_ly layout
#'
#' @export
plot_3d <- function(data) {
  relevant_cols <- names(data)[c(10,12,14,16)]

  data_clean <- data %>%
    dplyr::select(`What is your profession?`,
                  `How many years of expertise do you have?`,
                  `What is your nationality?`,
                  tidyselect::all_of(relevant_cols)) %>%
    dplyr::mutate(dplyr::across(tidyselect::all_of(relevant_cols),
                                ~ stringr::str_replace_all(., "%", "") %>%
                                  stringr::str_replace_all(",", ".") %>%
                                  as.numeric()))

  data_long <- data_clean %>%
    tidyr::pivot_longer(cols = tidyselect::all_of(relevant_cols),
                        names_to = "Question", values_to = "Value") %>%
    dplyr::filter(!is.na(Value)) %>%
    dplyr::mutate(Month = extract_label(Question))

  month_levels <- unique(data_long$Month)[order(match(unique(data_long$Month), extract_label(relevant_cols)))]

  data_long <- data_long %>%
    dplyr::mutate(Month = factor(Month, levels = month_levels))

  agg_data <- data_long %>%
    dplyr::group_by(
      `What is your profession?`,
      `How many years of expertise do you have?`,
      `What is your nationality?`,
      Month
    ) %>%
    dplyr::summarise(
      Median_Expectation = median(Value, na.rm = TRUE),
      Mean_Expectation = mean(Value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::rename(
      Profession = `What is your profession?`,
      Experience = `How many years of expertise do you have?`,
      Nationality = `What is your nationality?`
    ) %>%
    dplyr::mutate(
      Experience = factor(Experience, levels = c("0 - 5 years", "5 - 15 years", "over 15 years")),
      Nationality = factor(Nationality, levels = c("Slovak", "Non-Slovak")),
      Profession = factor(Profession)
    )

  monthly_data <- split(agg_data, agg_data$Month)

  plots <- lapply(names(monthly_data), function(month_name) {
    month_data <- monthly_data[[month_name]]

    plotly::plot_ly(
      month_data,
      x = ~Nationality,
      y = ~Experience,
      z = ~Profession,
      type = "scatter3d",
      mode = "markers",
      marker = list(
        size = 6,
        color = ~Median_Expectation,
        colorscale = "Reds",
        showscale = TRUE
      ),
      text = ~paste(
        "Nationality: ", Nationality, "<br>",
        "Experience: ", Experience, "<br>",
        "Profession: ", Profession, "<br>",
        "Median (", month_name, "): ", round(Median_Expectation, 2), "<br>",
        "Mean (", month_name, "): ", round(Mean_Expectation, 2)
      ),
      hoverinfo = "text"
    ) %>%
      plotly::layout(
        title = paste("3D Median Expectations –", month_name),
        scene = list(
          xaxis = list(title = "Nationality"),
          yaxis = list(title = "Experience"),
          zaxis = list(title = "Profession")
        )
      )
  })

  return(plots)
}
