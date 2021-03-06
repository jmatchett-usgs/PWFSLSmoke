
# ----- Example datasets ------------------------------------------------------

#' Carmel Valley example dataset
#' 
#' @docType data
#' @keywords datasets
#' @name Carmel_Valley
#' @title Carmel Valley Example Dataset
#' @format A list with two elements
#' @description
#' In August of 2016, the Soberanes fire in California burned along the Big Sur
#' coast. It was at the time the most expensive wildifre in US history. This dataset contains
#' PM2.5 monitoring data for the monitor in Carmel Valley which shows heavy smoke
#' as well as strong diurnal cycles associated with sea breezes. Data are stored
#' as a \emph{ws_monitor} object and are used in some examples in the package
#' documentation.
NULL

#' Northwest_Megafires example dataset
#' 
#' @docType data
#' @keywords datasets
#' @name Northwest_Megafires
#' @title Northwest Megafires Example Dataset
#' @format A list with two elements
#' @description
#' In the summer of 2015 Washington state had several catastrophic wildfires that led
#' to many days of heavy smoke in eastern Washington, Oregon and northern Idaho.
#' The Northwest_Megafires dataset contains AirNow ambient monitoring data for the 
#' Pacific Northwest from May 31 through November 01, 2015 (UTC). Data are stored
#' as a \emph{ws_monitor} object and are used in many examples in the package
#' documentation.
NULL

# ----- WRCC related info -----------------------------------------------------

#' WRCC monitor names and unitIDs
#' 
#' @docType data
#' @keywords datasets
#' @name WRCC
#' @title WRCC Monitor Names and Unit IDs
#' @format A list of lists
#' @description
#' The WRCC \url{http://www.wrcc.dri.edu/cgi-bin/smoke.pl}{Fire Cache Smoke Monitor Archive}
#' provides access to a variety of monitors that can be accessed with the \link{wrcc_createMonitorObject}
#' function. Use of this funciton requires a valid unitID. The \code{WRCC} object is 
#' a list of lists. The element named \code{unitIDs} is itself a list of three named vectors, 
#' each containing the unitIDs and associated names for
#' one of the categories of monitors available at WRCC:
#' 
#' \itemize{
#' \item{cache}
#' \item{miscellaneous}
#' \item{usfs_regional}
#' }
#' @note This list of monitor types was created on Feb 09, 2017.
NULL

# ----- AIRSIS related info ---------------------------------------------------

#' AIRSIS monitor types and codes
#' 
#' @export
#' @docType data
#' @name AIRSIS
#' @title AIRSIS Unit Types
#' @format A list of lists
#' @description
#' AIRSIS provides access to data by unit type at URLs like:
#'   http://usfs.airsis.com/vision/common/CSVExport.aspx?utid=38&StartDate=2017-11-06&EndDate=2017-11-07
#' 
#' The \code{AIRSIS} objectis a list of lists. The element named \code{unitTypes} is itself
#' a list of named unit types:
#' 
#' Unit types include:
#' \itemize{
#' \item{DATARAM}{ 21 = Dataram}
#' \item{BAM1020}{ 24 = Bam 1020}
#' \item{EBAM_NEW}{ 30 = eBam-New}
#' \item{EBAM}{ 38 = Iridium - Ebam}
#' \item{ESAM}{ 39 = Iridium - Esam}
#' \item{AUTOMET}{ 43 = Automet}
#' }
#' @note This list of monitor types was created on Feb 09, 2017.
AIRSIS <- list(unitTypes=list(DATARAM=21,
                              BAM1020=24,
                              EBAM_NEW=30,
                              EBAM=38,
                              ESAM=39,
                              AUTOMET=43))


# ----- AQI breaks -------------------------------------------------------------

#' AQI breaks and associated names and colors
#' 
#' @export
#' @docType data
#' @name AQI
#' @title Official Air Quality Index Levels, Names and Colors
#' @format A list with five elements
#' @description
#' Official AQI levels, names and colors are provided in a list for easy coloring and labeling.
#' @details
#' 
#' AQI breaks and colors are defined in
#' \url{https://www3.epa.gov/airnow/aqi-technical-assistance-document-may2016.pdf}
#' @note
#' The low end of each break category is used as the breakpoint.

AQI <- list(breaks_24=c(-Inf, 12, 35.5, 55.5, 150.5, 250.5, Inf),
            colors=c(grDevices::rgb(0,228/255,0),
                     grDevices::rgb(255/255,255/255,0),
                     grDevices::rgb(255/255,126/255,0),
                     grDevices::rgb(255/255,0,0),
                     grDevices::rgb(143/255,63/255,151/255),
                     grDevices::rgb(126/255,0,35/255)),
            names=c('good','moderate','USG','unhealthy','very unhealthy','hazardous'))

# ----- State codes -----------------------------------------------------------

#' CONUS state codes
#' 
#' @export
#' @docType data
#' @name CONUS
#' @title CONUS State Codes
#' @format A vector with 49 elements
#' @description
#' State codes for the 48 contiguous states +DC that make up the CONtinental US

CONUS <- c(     "AL","AZ","AR","CA","CO","CT","DE","FL","GA",
                "ID","IL","IN","IA","KS","KY","LA","ME","MD",
           "MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ",
           "NM","NY","NC","ND","OH","OK","OR","PA","RI","SC",
           "SD","TN","TX","UT","VT","VA","WA","WV","WI","WY",
           "DC"     )

#' US state codes
#' 
#' @export
#' @docType data
#' @name US_52
#' @title US State Codes
#' @format A vector with 52 elements
#' @description
#' State codes for the 50 states +DC +PR (Puerto Rico)

US_52 <- c("AK","AL","AZ","AR","CA","CO","CT","DE","FL","GA",
           "HI","ID","IL","IN","IA","KS","KY","LA","ME","MD",
           "MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ",
           "NM","NY","NC","ND","OH","OK","OR","PA","RI","SC",
           "SD","TN","TX","UT","VT","VA","WA","WV","WI","WY",
           "DC","PR")

