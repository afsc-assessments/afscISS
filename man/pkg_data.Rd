% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\name{pkg_data}
\alias{pkg_data}
\title{Developer function to get surveyISS results transferred to data package}
\usage{
pkg_data(region = c("ai", "ebs", "ebs_slope", "goa", "nebs"))
}
\arguments{
\item{region}{region for surveyISS output to be gathered (default = standard afsc survey regions)}
}
\value{
.rda files within /data folder
}
\description{
Function that creates package data for afscISS (NOTE: this is not a user fcn, this is a developer/maintainer fcn)
}
\keyword{internal}
