#' Extract and standardize month-year labels from survey question names
#'
#' This function extracts a full month-year (e.g., "January 2023") from a character string
#' and replaces it with its abbreviated form (e.g., "Jan 2023"). If no date is found, it returns
#' "Long-Run" if the string suggests a long-term horizon, or "Other" otherwise.
#'
#' @param question A character vector containing question labels.
#'
#' @return A character vector with standardized labels.
#'
#' @author Ole Paech
#' @export
extract_label <- function(question) {
  month_year <- stringr::str_extract(
    question,
    "(January|February|March|April|May|June|July|August|September|October|November|December)\\s\\d{4}"
  )

  ifelse(
    !is.na(month_year),
    stringr::str_replace_all(month_year, c(
      "January" = "Jan", "February" = "Feb", "March" = "Mar", "April" = "Apr",
      "May" = "May", "June" = "Jun", "July" = "Jul", "August" = "Aug",
      "September" = "Sep", "October" = "Oct", "November" = "Nov", "December" = "Dec"
    )),
    ifelse(
      stringr::str_detect(
        stringr::str_to_lower(question),
        "long[- ]?run|long[- ]?term"
      ),
      "Long-Run",
      "Other"
    )
  )
}

