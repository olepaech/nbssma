load_risk_files <- function(month_label) {
  base_path <- "C:/Users/olepa/OneDrive/Desktop/NBS/Answers/Archive"
  file_name <- paste0("Answers ", month_label, ".xlsx")
  full_path <- file.path(base_path, file_name)
  
  if (!file.exists(full_path)) {
    warning(paste("File not found:", full_path))
    return(NULL)
  }
  
  return(full_path)
}
