% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/airsis_createRawDataframe.R
\name{airsis_createRawDataframe}
\alias{airsis_createRawDataframe}
\title{Obain AIRSIS Data and Create a Raw Tibble}
\usage{
airsis_createRawDataframe(startdate = strftime(lubridate::now(), "\%Y010100",
  tz = "UTC"), enddate = strftime(lubridate::now(), "\%Y\%m\%d23", tz =
  "UTC"), provider = NULL, unitID = NULL, clusterDiameter = 1000,
  baseUrl = "http://xxxx.airsis.com/vision/common/CSVExport.aspx?",
  saveFile = NULL, flagAndKeep = FALSE)
}
\arguments{
\item{startdate}{desired start date (integer or character representing YYYYMMDD[HH])}

\item{enddate}{desired end date (integer or character representing YYYYMMDD[HH])}

\item{provider}{identifier used to modify baseURL \code{['APCD'|'USFS']}}

\item{unitID}{character or numeric AIRSIS unit identifier}

\item{clusterDiameter}{diameter in meters used to determine the number of clusters (see \code{addClustering})}

\item{baseUrl}{base URL for data queries}

\item{saveFile}{optional filename where raw CSV will be written}

\item{flagAndKeep}{flag, rather then remove, bad data during the QC process}
}
\value{
Raw tibblee of AIRSIS data.
}
\description{
Obtains monitor data from an AIRSIS webservice and converts
it into a quality controlled, metadata enhanced "raw" tibble
ready for use with all \code{raw_~} functions.

Steps involved include:

\enumerate{
 \item{download CSV text}
 \item{parse CSV text}
 \item{apply quality control}
 \item{apply clustering to determine unique deployments}
 \item{enhance metadata to include: elevation, timezone, state, country, site name}
}
}
\note{
The downloaded CSV may be saved to a local file by providing an argument to the \code{saveFile} parameter.
}
\examples{
\dontrun{
raw <- airsis_createRawDataframe(startdate=20160901, provider='USFS', unitID='1033')
raw <- raw_enhance(raw)
rawPlot_timeseries(raw,tlim=c(20160908,20160917))
}

}
\seealso{
\code{\link{airsis_downloadData}}

\code{\link{airsis_parseData}}

\code{\link{airsis_qualityControl}}

\code{\link{addClustering}}
}
\keyword{AIRSIS}
