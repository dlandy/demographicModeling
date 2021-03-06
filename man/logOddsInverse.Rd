% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilityFunctions.R
\name{logOddsInverse}
\alias{logOddsInverse}
\title{logOddsInverse}
\usage{
logOddsInverse(x)
}
\arguments{
\item{x}{a vector of positive values, between 0 and inf}
}
\value{
a vector containing transformed values
}
\description{
This function is a transformation function that inverts the log odds.
It's appropriate for mapping (-inf, inf) :->  (0, 1)
}
\examples{
logOdds(1:100/100)
}
