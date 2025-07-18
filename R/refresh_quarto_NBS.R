#' Render Quarto Website or Specific Indicator File
#'
#' Renders the full Quarto website or a single indicator `.qmd` file, depending on the input.
#'
#' @param indicator Optional character string. Name of indicator file (without extension).
#' @param open Logical. Whether to open the rendered HTML in a browser. Defaults to TRUE.
#' @param background_job Logical. Run rendering as background job. Defaults to FALSE.
#'
#' @author Martin Stefan (Deutsche Bundesbank), Ole Paech
#' @export
refresh_quarto <- function(indicator = NULL, open = TRUE, background_job = FALSE) {
  if (is.null(indicator)) {
    quarto::quarto_render("C:/Users/olepa/OneDrive/Desktop/NBS/Survey of Monetary Analysis", as_job = background_job) |>
      suppressMessages()
  } else {

    indicator <- tolower(indicator)

    path <- "C:/Users/olepa/OneDrive/Desktop/NBS/Survey of Monetary Analysis"
    file <- paste0(path, "/", indicator, ".qmd")
    exists <- file.exists(file)

    if (exists) {
      quarto::quarto_render(file, as_job = background_job) |>
        suppressMessages()
      if (!is.null(ref_file)) {
        quarto::quarto_render(ref_file, as_job = background_job) |> suppressMessages()
      }
    } else {
      stop(cat("You typed:", indicator, "\nIs that a valid indicator?\nThere is no file called", file, "\n"))
    }
  }

  if (open) {
    browseURL("C:/Users/olepa/OneDrive/Desktop/NBS/Survey of Monetary Analysis/_site/index.html")
  }
}
