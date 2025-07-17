library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(plotly)

path <- load_participant_files("Jul 25")
data <- read_excel(path)

plot_3d <- function(data) {
  relevant_cols <- names(data)[c(10,12,14,16)]
  
  data_clean <- data %>%
    select(`What is your profession?`, 
           `How many years of expertise do you have?`, 
           `What is your nationality?`, 
           all_of(relevant_cols)) %>%
    mutate(across(all_of(relevant_cols), ~ str_replace_all(., "%", "") %>%
                    str_replace_all(",", ".") %>%
                    as.numeric()))
  data_long <- data_clean %>%
    pivot_longer(cols = all_of(relevant_cols), names_to = "Question", values_to = "Value") %>%
    filter(!is.na(Value)) %>%
    mutate(Month = extract_label(Question))
  month_levels <- unique(data_long$Month)[order(match(unique(data_long$Month), extract_label(relevant_cols)))]
  data_long <- data_long %>%
    mutate(Month = factor(Month, levels = month_levels))
  
  agg_data <- data_long %>%
    group_by(
      `What is your profession?`, 
      `How many years of expertise do you have?`, 
      `What is your nationality?`, 
      Month
    ) %>%
    summarise(
      Median_Expectation = median(Value, na.rm = TRUE),
      Mean_Expectation = mean(Value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename(
      Profession = `What is your profession?`,
      Experience = `How many years of expertise do you have?`,
      Nationality = `What is your nationality?`
    ) %>%
    mutate(
      Experience = factor(Experience, levels = c("0 - 5 years", "5 - 15 years", "over 15 years")),
      Nationality = factor(Nationality, levels = c("Slovak", "Non-Slovak")),
      Profession = factor(Profession)
    )
  
  monthly_data <- split(agg_data, agg_data$Month)
  
  plots <- lapply(names(monthly_data), function(month_name) {
    month_data <- monthly_data[[month_name]]
    
    plot_ly(
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
      layout(
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

