disagreement_bar <- function(combined_results) {
  plots <- purrr::map(names(combined_results), function(name) {
    df <- combined_results[[name]]
    df$P75 <- as.numeric(df$P75)
    df$P25 <- as.numeric(df$P25)
    
    df <- df %>%
      mutate(Disagreement = P75 - P25)
    df$Date <- factor(df$Date, levels = unique(df$Date))
    
    p <- ggplot(df, aes(x = Date, y = Disagreement, group = 1)) +
      geom_col(fill = "#e41a1c") + 
      labs(title = paste("Disagreement (IQR) –", name),
           x = "Datum", y = "P75 - P25") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    return(p)
  })
  
  names(plots) <- names(combined_results)
  return(plots)
}