library(tidyverse)
library(readxl)

path <- load_participant_files("July 25")
data <- read_excel(path)


shaded_plot <- function(data) {
  suppressWarnings({
  relevant_cols <- names(data)[c(10,12,14,16)]
  
  data_clean <- data %>%
    select(all_of(relevant_cols)) %>%
    mutate(across(everything(), ~ .x %>%
                    str_replace_all("%", "") %>%
                    str_replace_all(",", ".") %>%
                    as.numeric()))
  
  data_long <- data_clean %>%
    pivot_longer(cols = everything(), names_to = "Question", values_to = "Rate") %>%
    filter(!is.na(Rate)) %>%
    mutate(Month = extract_label(Question))
  
  summary_stats <- data_long %>%
    group_by(Month) %>%
    summarise(
      Median = median(Rate, na.rm = TRUE),
      P25 = quantile(Rate, 0.25, na.rm = TRUE),
      P75 = quantile(Rate, 0.75, na.rm = TRUE)
    ) %>%
    mutate(Month_date = parse_date_time(Month, orders = "b Y")) %>%  
    arrange(Month_date) %>%
    mutate(Month_numeric = row_number()) %>%
    select(-Month_date)
  
  x_vals <- summary_stats$Month_numeric
  x_labels <- summary_stats$Month
  
  plot_ly() %>%
    add_trace(
      x = c(x_vals, rev(x_vals)),
      y = c(summary_stats$P25, rev(summary_stats$P75)),
      type = 'scatter',
      mode = 'none',
      fill = 'toself',
      fillcolor = 'rgba(173, 216, 230, 0.4)',
      hoverinfo = 'skip',  
      name = 'Interquartile Range'
    ) %>%

    add_trace(
      x = x_vals,
      y = summary_stats$Median,  
      type = 'scatter',
      mode = 'markers',
      marker = list(opacity = 0), 
      hoverinfo = 'text',
      text = paste0(
        x_labels,
        "<br>P25: ", round(summary_stats$P25, 2),
        "<br>P75: ", round(summary_stats$P75, 2)
      ),
      showlegend = FALSE
    ) %>%
    add_trace(
      x = x_vals,
      y = summary_stats$Median,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(color = 'blue', width = 2),
      marker = list(color = 'blue', size = 8),
      hoverinfo = 'text',
      text = paste0("Median: ", round(summary_stats$Median, 2)),
      name = 'Median'
    ) %>%
    layout(
      xaxis = list(
        tickmode = 'array',
        tickvals = x_vals,
        ticktext = x_labels,
        title = ''
      ),
      yaxis = list(title = 'Rate (in %)'),
      hovermode = 'x unified',
      showlegend = FALSE
    )
  })
}