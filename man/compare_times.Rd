% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compare_tidal_times.R
\name{compare_times}
\alias{compare_times}
\title{Compares a name of a sentinel file with the tide from that date}
\usage{
compare_times(sat_info, time_window, country = "fr", site_id = NULL)
}
\arguments{
\item{sat_info}{Name of a Sentinel file, the date will be extracted from here}

\item{time_window}{Time window (in minutes) around low tide for selecting an image}

\item{country}{code for the country tides. Currently fr for France and ca for Canada}

\item{site_id}{numerical code to the tidal station, e.g. 114 for Le Croisic}
}
\value{
a list with the tidal info and an element TRUE/FALSE if it fits the selection criteria (time_window)
}
\description{
Compares a name of a sentinel file with the tide from that date and if
it is within the time window defined by the user return TRUE
}
