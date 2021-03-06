% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/airnow_loadLatest.R
\name{airnow_loadLatest}
\alias{airnow_loadLatest}
\title{Load Most Recent Processed AirNow Monitoring Data}
\usage{
airnow_loadLatest(parameter = "PM2.5",
  baseUrl = "https://haze.airfire.org/monitoring/AirNow/RData/")
}
\arguments{
\item{parameter}{parameter of interest}

\item{baseUrl}{base URL for AirNow data}
}
\value{
A \emph{ws_monitor} object with AirNow data.
}
\description{
Loads pre-generated .RData files containing the most recent AirNow data.

AirNow parameters include the following:
\enumerate{
\item{PM2.5}
}

Avaialble RData and associated log files can be seen at:
\href{https://haze.airfire.org/monitoring/AirNow/RData/latest}{https://haze.airfire.org/monitoring/AirNow/RData/latest}
}
\examples{
\dontrun{
airnow <- airnow_loadLatest()
ca_mean <- monitor_subset(airnow, stateCodes='CA') \%>\%
           monitor_collapse()
monitorPlot_timeseries(ca_mean, shadedNight=TRUE)
}
}
\seealso{
\code{\link{airnow_load}}

\code{\link{airnow_loadDaily}}
}
\keyword{AirNow}
