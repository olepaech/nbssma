#' Scrape the current ECB Deposit Facility Rate (DFR)
#'
#' Downloads and parses the current ECB Deposit Facility Rate from
#' TradingEconomics.com using a fixed XPath.
#'
#' @return A numeric value representing the current DFR (in percent).
#'
#' @examples
#' \dontrun{
#' get_current_dfr()
#' }
#'
#'
#' @importFrom rvest read_html html_element html_text
#'
#' @author Ole Paech
#'
#' @export
get_current_dfr <- function() {
  url <- "https://tradingeconomics.com/euro-area/indicators"

  page <- rvest::read_html(url)

  node <- rvest::html_element(
    page,
    xpath = '//*[@id="money"]/div/div/table/tbody/tr[15]/td[2]'
  )

  text <- rvest::html_text(node)

  value <- base::trimws(text) |> base::as.numeric()

  return(value)
}
