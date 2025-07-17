library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(plotly)
library(purrr)

create_risk_share_plot <- function(file_paths_named_list, type = "Upside") {
  # Bewertungsmap
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
    df <- read_excel(path)
    
    df <- df %>%
      mutate(
        Inflation = df[[18]] %>%
          as.character() %>%
          str_replace_all("%", "") %>%
          str_replace_all(",", ".") %>%
          as.numeric()
      ) %>%
      select(Inflation, 19:28) %>%
      mutate(across(2:11, ~ importance_map[.])) %>%
      mutate(Source = label)
    
    return(df)
  }
  
  all_data <- purrr::map2_dfr(file_paths_named_list, names(file_paths_named_list), process_file)
  
  all_data <- all_data %>%
    mutate(Source = factor(Source, levels = names(file_paths_named_list)))
  
  if (type == "Upside") {
    selected_cols <- 2:6
    prefix <- "Upside"
    title <- "Composition of Upside Risk Over Time"
  } else {
    selected_cols <- 7:11
    prefix <- "Downside"
    title <- "Composition of Downside Risk Over Time"
  }
  
  summary_data <- all_data %>%
    group_by(Source) %>%
    summarise(across(all_of(selected_cols), ~ mean(., na.rm = TRUE), .names = paste0(prefix, "_", "{col}")))
  
  risk_shares <- summary_data %>%
    pivot_longer(-Source, names_to = "Risk", values_to = "Value") %>%
    mutate(Risk = str_remove(Risk, paste0("^", prefix, "_"))) %>%
    group_by(Source) %>%
    mutate(Share = Value / sum(Value, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(text = paste0("Risk: ", Risk, "\nDate: ", Source, "\nShare: ", round(Share * 100, 1), "%"))
  
  farben <- c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e")
  risks <- sort(unique(risk_shares$Risk))
  farben_named <- setNames(rep_len(farben, length(risks)), risks)
  
  p <- ggplot(risk_shares, aes(x = Source, y = Share, fill = Risk, text = text)) +
    geom_col(position = "fill", color = "black", width = 0.7) +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_fill_manual(values = farben_named) +
    labs(
      title = title,
      x = "Survey Date",
      y = paste("Share of Total", type, "Risk"),
      fill = paste(type, "Risk")
    ) +
    theme_minimal() +
    theme(
      text = element_text(size = 14),
      plot.title = element_text(face = "bold", hjust = 0.5)
    )
  
  return(ggplotly(p, tooltip = "text"))
}

file_paths <- prepare_file_list(c("May 25", "Jun 25"))

create_risk_share_plot(file_paths, type = "Upside")
create_risk_share_plot(file_paths, type = "Downside")
