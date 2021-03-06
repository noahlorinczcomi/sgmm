% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build.lcmm.R
\name{build.lcmm}
\alias{build.lcmm}
\title{A LCGA-Building Function}
\usage{
build.lcmm(data, x_name, y_name, g_name, link, ng = 2)
}
\arguments{
\item{data}{enter data set.}

\item{x_name}{enter column name (str) of time-indicating variable.}

\item{y_name}{enter column name (str) of time-varying outcome variable.}

\item{g_name}{enter subject grouping column name (str; usually 'ID').}

\item{ng}{enter number of latent classes you would like to search for.}
}
\description{
This function helps you create a limited-specs latent class growth analysis model using 'lcmm.'
}
\examples{
build.lcmm()
}
\keyword{lcmm}
