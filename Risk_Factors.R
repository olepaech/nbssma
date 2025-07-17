
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)


path <- load_participant_files("Jul 25")
data <- read_excel(path)

risk_factors_bars <- function(df){
  suppressWarnings({
    inflation_col <- names(df)[18]
    risks_1 <- names(df)[19:23]  
    risks_2 <- names(df)[24:28]  
    
    
    mapping <- c(
      "Absolutely no relevance" = 0,
      "Not so Important" = 1,
      "Moderate" = 2,
      "Important" = 3,
      "Very Important" = 4
    )
    
    df_clean <- df %>%
      mutate(
        inflation_raw = .[[inflation_col]],
        inflation_clean_char = gsub(",", ".", gsub("%", "", inflation_raw)),
        inflation_value = suppressWarnings(as.numeric(trimws(inflation_clean_char)))
      ) %>%
      mutate(across(all_of(c(risks_1, risks_2)), ~ mapping[trimws(as.character(.))], .names = "num_{.col}"))
    
    risk1_means <- colMeans(df_clean %>% select(starts_with("num_") & all_of(paste0("num_", risks_1))), na.rm = TRUE)
    risk2_means <- colMeans(df_clean %>% select(starts_with("num_") & all_of(paste0("num_", risks_2))), na.rm = TRUE)
    inflation_value <- mean(df_clean$inflation_value, na.rm = TRUE)
    
    df_plot <- data.frame(
      Variable = c(risks_1, risks_2),
      Value = c(risk1_means, -risk2_means),  # X-AB wird negativ
      Group = c(rep("S–W", length(risks_1)), rep("X–AB", length(risks_2)))
    )
    
    farben <- c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e",
                "#a6761d", "#666666", "#e6ab02", "#1f78b4", "#b2df8a")
    df_plot$Variable <- factor(df_plot$Variable, levels = c(risks_1, risks_2))
    
    df_plot <- df_plot %>%
      arrange(Group, -Value) %>%
      group_by(Group) %>%
  mutate(
    ymin = cumsum(lag(Value, default = 0)),
    ymax = ymin + Value
  ) %>%
  ungroup() %>%
  mutate(
    ymin = inflation_value + ymin,
    ymax = inflation_value + ymax,
    x = ifelse(Group == "S–W", "Upside Risks", "Downside Risks")
  ) %>%
  group_by(Group) %>%
  mutate(share = round(abs(Value) / sum(abs(Value)) * 100, 1)) %>%
  ungroup()

total_upside <- round(sum(df_plot$Value[df_plot$Group == "S–W"]),2)
total_downside <- round(sum(abs(df_plot$Value[df_plot$Group == "X–AB"])),2)

p <- ggplot(df_plot, aes(x = x, ymin = ymin, ymax = ymax, fill = Variable,
                         text = paste0(Variable, ": ", share, "%"))) +
  geom_rect(aes(xmin = 0.7, xmax = 1.3), color = "black") +
  geom_hline(yintercept = inflation_value, linetype = "dashed", linewidth = 1.1) +
  geom_point(aes(x = 1, y = inflation_value,
                 text = paste0("Average Inflation Expectation: ", round(inflation_value, 2))),
             color = "transparent") +
  annotate("text",
           x = 1,
           y = max(df_plot$ymax[df_plot$Group == "S–W"]) + 1.5,
           label = paste0("Upside Risk: ", total_upside),
           size = 4, fontface = "bold", hjust = 0.5) +
  
  annotate("text",
           x = 1,
           y = min(df_plot$ymin[df_plot$Group == "X–AB"]) - 5.0,
           label = paste0("Downside Risk: ", total_downside),
           size = 4, fontface = "bold", hjust = 0.5) +
  
  scale_fill_manual(values = farben) +
  scale_y_continuous(name = "Average Inflation Expectation") +
  theme_minimal() +
  labs(x = NULL) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    legend.position = "right",  
    legend.title = element_blank()
  )


ggplotly(p, tooltip = "text")
  })
}