median_barplot <- function(combined_results) {
  plots <- purrr::map(names(combined_results), function(name) {
    df <- combined_results[[name]]
    df$Date <- factor(df$Date, levels = unique(df$Date))
    
    p <- ggplot(df, aes(x = Date, y = Median)) +
      geom_col(fill = "#377eb8") +
      labs(title = paste("Median der Teilnehmerwerte –", name),
           x = "Datum", y = "Median") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    return(p)
  })
  
  names(plots) <- names(combined_results)
  return(plots)
}