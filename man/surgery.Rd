% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/surgery.R
\name{surgery}
\alias{surgery}
\title{Create a data frame of surgical data from a tumor registry that can be combined into a storyboard}
\usage{
surgery(data)
}
\arguments{
\item{data}{is a data frame which contains the data for which you want to create a storyboard}
}
\value{
A data frame with five variables ("record_id", "description", "value", "date", and "hover" that can be combined with others dfs with the same five variables to form a storyboard
}
\description{
`surgery()`wrangles data from the Surgery form of tumor registries to produce a data frame of details about surgical therapy, which can then be incorporated into a Patient Storyboard
}
\examples{
# Test with embedded data set "storyboard_dataset"
storyboard_dataset \%>\%
  surgery()

}
