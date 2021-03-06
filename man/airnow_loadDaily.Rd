% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/airnow_loadDaily.R
\name{airnow_loadDaily}
\alias{airnow_loadDaily}
\title{Load Recent Processed AirNow Monitoring Data}
\usage{
airnow_loadDaily(parameter = "PM2.5",
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
Loads pre-generated .RData files containing recent AirNow data.

The daily files are generated once a day, shortly after midnight and contain data for the
previous 45 days. 

For the most recent data, use \code{airnow_loadLatest()}.

AirNow parameters include the following:
\enumerate{
\item{PM2.5}
}

Avaialble RData and associated log files can be seen at:
\href{https://haze.airfire.org/monitoring/AirNow/RData/latest}{https://haze.airfire.org/monitoring/AirNow/RData/latest}
}
\examples{
\dontrun{
airnow <- airnow_loadDaily()
airnow \%>\% monitor_subset(stateCodes=CONUS) \%>\% monitorMap()
}
}
\seealso{
\code{\link{airnow_load}}

\code{\link{airnow_loadLatest}}
}
\keyword{AirNow}
