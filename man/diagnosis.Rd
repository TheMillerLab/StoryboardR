% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/diagnosis.R
\name{diagnosis}
\alias{diagnosis}
\title{Wrangles data from a tumor registry regarding date of initial histological confirmation}
\usage{
diagnosis(data)
}
\arguments{
\item{data}{is a data frame which contains the data for which you want to create a storyboard}
}
\value{
A data frame with five variables ("record_id", "description", "value", "date", and "hover" that can be combined with others dfs with the same five variables to form a storyboard
}
\description{
`diagnosis()` wrangles data from a tumor registry regarding date of initial histological confirmation of the diagnosis, which can then be incorporated into a Patient Storyboard
}
\examples{
# Test with embedded data set "storyboard_dataset"
storyboard_dataset \%>\%
  diagnosis()
}
