% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hurricane.R
\name{stormObservation}
\alias{stormObservation}
\title{Subset to the specific hurricane that you will be mapping (Hurricane Ike) and to a single observation
time for that hurricane}
\usage{
stormObservation(storm_name, date_time)
}
\arguments{
\item{storm_name}{a string of the storm name (e.g. "Ike")}

\item{date_time}{the time of the observation}
}
\value{
stormdata data frame with a single observation
}
\description{
Subset hurricane data to single storm and observation
}
\details{
This function takes a storm name and year and returns a single observation for the
storm at a specific moment in time.
}
