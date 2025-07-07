library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(zoo)
library(stringr)

combine_survey_results <- function(subfolder) {
  
  extract_block <- function(df, start_row, end_row) {
    df_block <- df[(start_row + 1):(end_row - 1), ]
    colnames(df_block) <- c("Date", "DFR", "MRO", "MLF", "€STR", "3-months EURIBOR")
    df_block <- df_block %>% filter(!if_all(everything(), is.na))
    return(df_block)
  }
  
  parse_participant_file <- function(file) {
    raw <- raw <- suppressMessages(read_excel(file, col_names = FALSE))
    start_gc <- which(raw[[1]] == "GC meetings")
    start_q  <- which(raw[[1]] == "Quarters")
    start_lr <- which(raw[[1]] == "Long run")
    end_gc <- start_q
    end_q  <- start_lr
    end_lr <- nrow(raw) + 1
    list(
      gc       = extract_block(raw, start_gc, end_gc),
      quarters = extract_block(raw, start_q, end_q),
      longrun  = extract_block(raw, start_lr, end_lr)
    )
  }
  
  load_participant_files <- function(subfolder) {
    base_path <- "C:/Users/olepa/OneDrive/Desktop/NBS/Answers"
    full_path <- file.path(base_path, subfolder)
    files <- list.files(full_path, pattern = "\\.xlsx$", full.names = TRUE)
    return(files)
  }
  
  combine_field <- function(section, field) {
    df_list <- map2(all_data, participant_names, function(participant, name) {
      df <- participant[[section]]
      df <- df %>%
        select(Date = 1, value = all_of(field)) %>%
        mutate(across(value, as.numeric)) %>%
        rename(!!name := value)
    })
    reduce(df_list, full_join, by = "Date")
  }
  
  files <- load_participant_files(subfolder)
  participant_names <- paste0("Teilnehmer_", seq_along(files))
  all_data <- map(files, parse_participant_file)
  
  sections <- c("gc", "quarters", "longrun")
  fields <- c("DFR", "MRO", "MLF", "€STR", "3-months EURIBOR")
  
  combined_results <- list()
  for (sec in sections) {
    for (field in fields) {
      df_combined <- combine_field(sec, field)
      combined_results[[paste(sec, field, sep = "_")]] <- df_combined
    }
  }
  
  # Date-Feld für GC-Tabellen als yearmon formatieren
  gc_names <- grep("^gc_", names(combined_results), value = TRUE)
  for (name in gc_names) {
    combined_results[[name]]$Date <- as.yearmon(as.Date(as.numeric(combined_results[[name]]$Date), origin = "1899-12-30"))
  }
  add_statistics <- function(df) {
    df_stats <- df %>%
      rowwise() %>%
      mutate(
        P25 = quantile(as.numeric(c_across(starts_with("Teilnehmer_"))), 0.25, na.rm = TRUE),
        Median = median(as.numeric(c_across(starts_with("Teilnehmer_"))), na.rm = TRUE),
        P75 = quantile(as.numeric(c_across(starts_with("Teilnehmer_"))), 0.75, na.rm = TRUE)
      ) %>%
      ungroup()
    return(df_stats)
  }
  combined_results <- map(combined_results, add_statistics)
  return(combined_results)
}