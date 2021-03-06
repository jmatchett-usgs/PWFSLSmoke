% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/airsis_BAM1020QualityControl.R
\name{airsis_BAM1020QualityControl}
\alias{airsis_BAM1020QualityControl}
\title{Apply Quality Control to Raw AIRSIS BAM1020 Dataframe}
\usage{
airsis_BAM1020QualityControl(tbl, valid_Longitude = c(-180, 180),
  valid_Latitude = c(-90, 90), remove_Lon_zero = TRUE,
  remove_Lat_zero = TRUE, valid_Flow = c(0.834 * 0.95, 0.834 * 1.05),
  valid_AT = c(-Inf, 45), valid_RHi = c(-Inf, 45), valid_Conc = c(-Inf,
  5000), flagAndKeep = FALSE)
}
\arguments{
\item{tbl}{single site tibble created by \code{airsis_parseData()}}

\item{valid_Longitude}{range of valid Longitude values}

\item{valid_Latitude}{range of valid Latitude values}

\item{remove_Lon_zero}{flag to remove rows where Longitude == 0}

\item{remove_Lat_zero}{flag to remove rows where Latitude == 0}

\item{valid_Flow}{range of valid Flow values}

\item{valid_AT}{range of valid AT values}

\item{valid_RHi}{range of valid RHi values}

\item{valid_Conc}{range of valid ConcHr values}

\item{flagAndKeep}{flag, rather than remove, bad data during the QC process}
}
\value{
Cleaned up tibble of AIRSIS monitor data.
}
\description{
Perform various QC measures on AIRSIS BAM1020 data.

A \code{POSIXct datetime} column (UTC) is also added based on \code{DateTime}.
}
\seealso{
\code{\link{airsis_qualityControl}}
}
\keyword{AIRSIS}
