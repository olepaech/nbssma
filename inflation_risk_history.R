library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(plotly)

files <- list(
  "May 25" = load_risk_files("May 25"),
  "Jun 25" = load_risk_files("Jun 25"),
  "Current" = load_participant_files()
)


inflation_risk_history <- function(files_with_labels) {
  suppressWarnings({
    suppressMessages({
  importance_map <- c(
    "Absolutely no relevance" = 0,
    "Not so Important" = 1,
    "Moderate" = 2,
    "Important" = 3,
    "Very Important" = 4
  )
  
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
  
  all_data <- bind_rows(
    lapply(seq_along(files_with_labels), function(i) {
      process_file(files_with_labels[[i]], names(files_with_labels)[i])
    })
  )
  
  all_data <- all_data %>%
    mutate(Source = factor(Source, levels = names(files_with_labels)))
  
  summary_data <- all_data %>%
    group_by(Source) %>%
    summarise(
      inflation_exp = mean(Inflation, na.rm = TRUE),
      across(2:6, ~ mean(., na.rm = TRUE), .names = "Upside_{.col}"),
      across(7:11, ~ mean(., na.rm = TRUE), .names = "Downside_{.col}")
    )
  
  upside <- summary_data %>%
    select(Source, starts_with("Upside_")) %>%
    pivot_longer(-Source, names_to = "Risk", values_to = "Value") %>%
    mutate(Type = "Upside")
  
  downside <- summary_data %>%
    select(Source, starts_with("Downside_")) %>%
    pivot_longer(-Source, names_to = "Risk", values_to = "Value") %>%
    mutate(Type = "Downside")
  
  stack_data <- bind_rows(upside, downside)
  
  stack_data <- stack_data %>%
    left_join(summary_data %>% select(Source, inflation_exp), by = "Source")
  
  stack_data <- stack_data %>%
    group_by(Source, Type) %>%
    arrange(Source, Type, Risk) %>%
    mutate(
      Share = Value / sum(Value, na.rm = TRUE),
      Total = sum(Value, na.rm = TRUE),
      cum_share = cumsum(Share),
      ymin = ifelse(Type == "Upside",
                    inflation_exp + Total * (cum_share - Share),
                    inflation_exp - Total * cum_share),
      ymax = ifelse(Type == "Upside",
                    inflation_exp + Total * cum_share,
                    inflation_exp - Total * (cum_share - Share))
    ) %>%
    ungroup()
  
  colors <- c(
    "#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e",
    "#a6761d", "#666666", "#e6ab02", "#1f78b4", "#b2df8a"
  )
  
  unique_risks <- sort(unique(stack_data$Risk))
  stack_data$Risk <- factor(stack_data$Risk, levels = unique_risks)
  colors_named <- setNames(colors, levels(stack_data$Risk))
  
  inflation_points <- summary_data %>%
    mutate(x = as.numeric(factor(Source)),
           text = paste0("Inflation Expectation: ", round(inflation_exp, 2), "%"))
  
  totals_df <- stack_data %>%
    group_by(Source, Type) %>%
    summarise(
      Total = unique(Total),
      inflation_exp = unique(inflation_exp),
      x = as.numeric(unique(Source))
    ) %>%
    mutate(
      y = ifelse(Type == "Upside", inflation_exp + Total + 0.3, inflation_exp - Total - 0.3),
      label = paste0("Total ", Type, ": ", round(Total, 2))
    )
  
  p <- ggplot() +
    geom_rect(data = stack_data,
              aes(xmin = as.numeric(factor(Source)) - 0.3,
                  xmax = as.numeric(factor(Source)) + 0.3,
                  ymin = ymin,
                  ymax = ymax,
                  fill = Risk,
                  text = paste0(Risk, ": ", round(Share * 100, 1), "%")),
              color = "black") +
    
    geom_line(data = summary_data,
              aes(x = as.numeric(factor(Source)), y = inflation_exp, group = 1),
              color = "black", size = 1.2) +
    geom_point(data = inflation_points,
               aes(x = x, y = inflation_exp, text = text),
               color = "black", size = 2) +
    geom_text(data = totals_df %>% filter(Type == "Upside"),
              aes(x = x, y = y, label = label),
              size = 4, vjust = 0, fontface = "bold") +
    geom_text(data = totals_df %>% filter(Type == "Downside"),
              aes(x = x, y = y, label = label),
              size = 4, vjust = 1.2, fontface = "bold") +
    
    scale_x_continuous(breaks = 1:length(files_with_labels), labels = names(files_with_labels)) +
    scale_y_continuous(breaks = seq(0, ceiling(max(stack_data$ymax, na.rm = TRUE)), by = 2))+
    scale_fill_manual(values = colors_named) +
    labs(y = "Average Inflation Expectation",
         x = "Survey Date",
         fill = "Risk Factor") +
    theme_minimal() +
    theme(
      text = element_text(size = 14),
      plot.title = element_text(face = "bold", hjust = 0.5)
    )
  
  ggplotly(p, tooltip = "text")
  })
  })
}

inflation_risk_history(files)
