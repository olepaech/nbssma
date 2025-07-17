library(fmsb)
library(dplyr)
library(tidyr)
library(readxl)
library(stringr)
library(tibble)

path <- load_participant_files("Jul 25")
data <- read_excel(path)

spiderplot_category <- function(data, category, Min = 1, Max = 4){
  suppressWarnings({
    category_map <- list(
      "Profession" = "What is your profession?",
      "Experience" = "How many years of expertise do you have?",
      "Nationality" = "What is your nationality?"
    )
    
    if (!(category %in% names(category_map))) {
      stop("Invalid Category. Please choose between 'Profession', 'Experience' or 'Nationality'.")
    }
    
    category_col <- category_map[[category]]
    
    relevant_cols <- names(data)[c(10,12,14,16)]
    
    data_clean <- data %>%
      select(all_of(category_col), all_of(relevant_cols)) %>%
      mutate(across(all_of(relevant_cols), ~ str_replace_all(., "%", "") %>%
                      str_replace_all(",", ".") %>%
                      as.numeric()))
    
    data_long <- data_clean %>%
      pivot_longer(cols = all_of(relevant_cols), names_to = "Question", values_to = "Value") %>%
      filter(!is.na(Value)) %>%
      mutate(Month = extract_label(Question))
    month_levels <- unique(data_long$Month)[order(match(unique(data_long$Month), extract_label(relevant_cols)))]
    data_long <- data_long %>%
      mutate(Month = factor(Month, levels = month_levels)) %>%
      group_by(.data[[category_col]], Month) %>%
      summarise(
        Median_Expectation = median(Value),
        .groups = "drop"
      )
    
    
      radar_df <- data_long %>%
        pivot_wider(names_from = .data[[category_col]],
                    values_from = Median_Expectation)
      
      radar_df <- radar_df %>%
        mutate(across(-Month, as.numeric))
      max_row <- radar_df[1, ] %>%
        mutate(across(-Month, ~ Max), Month = "MAX")
      min_row <- radar_df[1, ] %>%
        mutate(across(-Month, ~ Min), Month = "MIN")
      
      radar_df <- bind_rows(max_row, min_row, radar_df)
      radar_df <- radar_df %>%
        column_to_rownames("Month")
      
      radarchart(
        radar_df,
        axistype = 1,
        pcol = rainbow(nrow(radar_df) - 2),
        plwd = 2,
        plty = 1,
        cglcol = "grey",
        cglty = 1,
        axislabcol = "grey",
        caxislabels = seq(1, 4, 0.5),
        vlcex = 0.8,
      )
      
      legend("topright", legend = rownames(radar_df)[-c(1, 2)],
             col = rainbow(nrow(radar_df) - 2), lty = 1, lwd = 2, bty = "n")
    })
}