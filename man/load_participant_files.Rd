% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_participant_files.R
\name{load_participant_files}
\alias{load_participant_files}
\title{Load Excel files from a given participant subfolder}
\usage{
load_participant_files(subfolder = "Current")
}
\arguments{
\item{subfolder}{A character string specifying the subfolder name within the base path.
Defaults to \code{"Current"}.}
}
\value{
A character vector of full file paths to \code{.xlsx} files.
}
\description{
This function constructs the full path to a directory containing participant Excel files
and returns all \code{.xlsx} file paths found in that directory. The base Path is: 'O:/OMP/a_IR_survey/'
}
\examples{
\dontrun{
  load_participant_files()              # Loads from "Current"
  load_participant_files("New Subforlder")      # Loads from subfolder "New Subfolder"
}

}
\author{
Ole Paech
}
