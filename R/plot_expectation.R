#' Plot expectations for the ECB Deposit Facility Rate
#'
#' Scrapes the current ECB Deposit Facility Rate (DFR) from TradingEconomics,
#' compares it to numeric expectations in a user-supplied data frame column,
#' and visualizes the direction of expected changes.
#'
#' @param data A data frame with expectation values (as character or numeric).
#' @param rel_col A numeric or character vector specifying the column(s) with expectations.
#'
#' @return A ggplot2 bar chart showing number of respondents expecting increase, decrease, or no change.
#'
#' @importFrom rvest read_html html_element html_text
#' @importFrom dplyr select all_of mutate across everything case_when
#' @importFrom stringr str_replace_all
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_bar scale_fill_manual scale_y_continuous
#' @importFrom ggplot2 labs theme_minimal
#' @importFrom scales expansion
#'
#'
#' @examples
#' \dontrun{
#' path <- load_participant_files()
#' data <- readxl::read_excel(path)
#' plot_dfr_expectations(data)
#' }
#'
#' @author Ole Paech
#'
#' @export
plot_dfr_expectations <- function(data, rel_col = c(10)) {
  get_current_dfr <- function() {
    url <- "https://tradingeconomics.com/euro-area/indicators"
    page <- rvest::read_html(url)
    node <- rvest::html_element(page, xpath = '//*[@id="money"]/div/div/table/tbody/tr[15]/td[2]')
    text <- rvest::html_text(node)
    value <- text |> base::trimws() |> base::as.numeric()
    return(value)
  }

  current_dfr <- get_current_dfr()
  relevant_cols <- base::names(data)[rel_col]
  month_labels <- extract_label(relevant_cols)

  data_numeric <- data |>
    dplyr::select(dplyr::all_of(relevant_cols)) |>
    dplyr::mutate(dplyr::across(
      dplyr::everything(),
      ~ .x |>
        stringr::str_replace_all("%", "") |>
        stringr::str_replace_all(",", ".") |>
        base::as.numeric()
    )) |>
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "Variable",
      values_to = "Expectation"
    ) |>
    dplyr::select(Expectation)

  df_compare <- data_numeric |>
    dplyr::mutate(
      Category = dplyr::case_when(
        Expectation > current_dfr ~ "Rate Increase",
        Expectation < current_dfr ~ "Rate Decrease",
        TRUE ~ "No Change"
      ),
      Category = base::factor(Category, levels = c("Rate Decrease", "No Change", "Rate Increase"))
    )

  p <- ggplot2::ggplot(df_compare, ggplot2::aes(x = Category, fill = Category)) +
    ggplot2::geom_bar() +
    ggplot2::scale_fill_manual(values = c(
      "Rate Increase" = "#1c355e",
      "Rate Decrease" = "#cce1ee",
      "No Change" = "#0067ab"
    )) +
    ggplot2::scale_y_continuous(
      breaks = function(x) base::floor(base::min(x)):base::ceiling(base::max(x)),
    ) +
    ggplot2::labs(
      title = paste0(
        "Expectations for the ECB Deposit Facility Rate for ",
        month_labels,
        " (Current: ", current_dfr, "%)"
      ),
      x = "Expected Rate Direction",
      y = "Number of Respondents"
    ) +
    ggplot2::theme_minimal(base_family = "Arial")

  return(p)
}
