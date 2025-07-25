% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hover_bars.R
\name{hover_barplot}
\alias{hover_barplot}
\title{Create interactive bar chart of DFR expectations}
\usage{
hover_barplot(
  data,
  rel_cols = c(10, 12, 14),
  xlab = "",
  ylab = "Median Rate (in \%)",
  title = ""
)
}
\arguments{
\item{data}{A data frame containing survey responses.}

\item{rel_cols}{A vector stating in which columns of the file the data to visualize are.}

\item{xlab}{A character string specifying the x-axis label (optional).}

\item{ylab}{A character string specifying the y-axis label (optional).}

\item{title}{A character string specifying the title of the graph (optional).}
}
\value{
A \code{plotly} interactive barplot object.
}
\description{
Generates a Plotly bar chart with median, mode, and percentile statistics for NBS survey data.
}
\examples{
\dontrun{
path <- load_participant_files()
data <- readxl::read_excel(path)
hover_barplot(data)
}

}
\author{
Ole Paech
}
