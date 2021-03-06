#' @keywords AIRSIS
#' @export
#' @title Load Recent AIRSIS Monitoring Data
#' @param baseUrl location of the AIRSIS latest data file
#' @description Loads pre-generated .RData files containing the most recent AIRSIS data.
#' 
#' Avaialble RData and associated log files can be seen at:
#' \href{https://haze.airfire.org/monitoring/AIRSIS/RData/latest}{https://haze.airfire.org/monitoring/AIRSIS/RData/latest}
#' @seealso \code{\link{airsis_load}}
#' @seealso \code{\link{airsis_loadDaily}}
#' @return A \emph{ws_monitor} object with AIRSIS data.
#' @examples
#' \dontrun{
#' airsis <- airsis_loadLatest()
#' }

airsis_loadLatest <- function(baseUrl='https://haze.airfire.org/monitoring/AIRSIS/RData/') {
  
  # Create filepath
  filepath <- paste0("latest/airsis_PM2.5_latest10.RData")
  
  # Define a 'connection' object so we can be sure to close it no matter what happens
  conn <- url(paste0(baseUrl,filepath))
  result <- try( suppressWarnings(ws_monitor <- get(load(conn))),
                 silent=TRUE )
  close(conn)
  
  if ( "try-error" %in% class(result) ) {
    stop(paste0("URL unavailable: ",paste0(baseUrl,filepath)), call.=FALSE)
  }
  
  return(ws_monitor)
}
