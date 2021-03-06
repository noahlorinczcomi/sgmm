% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stop.time.R
\name{stop.time}
\alias{stop.time}
\title{A Time-Making Function}
\usage{
stop.time(data, id_name, time_cutoff)
}
\arguments{
\item{data}{enter data for which each ID does NOT have the same beginning and end time-point.}

\item{id_name}{enter the name (str) of the column in your data containing subject's IDs.}

\item{time_cutoff}{enter the number of days, months, years that define each subject's final observation.}
}
\description{
This function helps you create final time-outcome values for known but unobserved values (e.g., dosages) at a user-specified point in time.
}
\examples{
stop.time()
}
\keyword{lcmm}
