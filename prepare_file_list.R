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

load_participant_files <- function(subfolder = "Current") {
  base_path <- "C:/Users/olepa/OneDrive/Desktop/NBS/Answers"
  full_path <- file.path(base_path, subfolder)
  files <- list.files(full_path, pattern = "\\.xlsx$", full.names = TRUE)
  return(files)
}

prepare_file_list <- function(month_labels = c("May 25", "Jun 25")) {
  month_files <- purrr::set_names(
    purrr::map(month_labels, load_risk_files),
    month_labels
  )
  
  # "Current" Dateien laden
  current_files <- load_participant_files()
  
  # Falls mehrere Dateien in "Current", alle hinzufügen; ansonsten nur eine
  # Benannte Liste erstellen: die Month-Dateien + "Current"
  
  # Falls mehrere, benenne mit Current_1, Current_2, ...
  if(length(current_files) > 1){
    current_named <- setNames(current_files, paste0("Current_", seq_along(current_files)))
  } else if(length(current_files) == 1){
    current_named <- setNames(current_files, "Current")
  } else {
    current_named <- NULL
  }
  
  # Alle zusammenfügen
  combined <- c(month_files, current_named)
  
  # NULL-Einträge (fehlende Dateien) entfernen
  combined <- combined[!sapply(combined, is.null)]
  
  return(combined)
}

file_paths <- prepare_file_list(c("May 25", "Jun 25"))