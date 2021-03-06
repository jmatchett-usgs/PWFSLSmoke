% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/epa_load.R
\name{epa_load}
\alias{epa_load}
\title{Load Processed EPA Monitoring Data}
\usage{
epa_load(year = strftime(lubridate::now(), "\%Y", tz = "UTC"),
  parameterCode = "88101",
  baseUrl = "https://haze.airfire.org/monitoring/EPA/RData/")
}
\arguments{
\item{year}{desired year (integer or character representing YYYY)}

\item{parameterCode}{pollutant code}

\item{baseUrl}{base URL for EPA .RData files}
}
\value{
A \emph{ws_monitor} object with EPA data for an entire year.
}
\description{
Loads a pre-generated .RData file containing a year's worth of
monitoring data.

EPA parameter codes include:
\enumerate{
\item{88101}{ -- PM2.5 FRM/FEM Mass (begins in 2008)}
\item{88502}{ -- PM2.5 non FRM/FEM Mass (begins in 1998)}
}

Avaialble RData and associated log files can be seen at:
\href{https://haze.airfire.org/monitoring/EPA/RData/}{https://haze.airfire.org/monitoring/EPA/RData/}
}
\examples{
\dontrun{
epa_frm <- epa_load(2015, 88101)
epa_frm_conus <- monitor_subset(epa_frm, stateCodes=CONUS)
monitorLeaflet(epa_frm_conus)
}
}
\references{
\href{https://aqs.epa.gov/aqsweb/airdata/download_files.html#Raw}{EPA AirData Pre-Generated Data Files}
}
\keyword{EPA}
