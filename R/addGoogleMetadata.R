#' @keywords internal
#' @export
#' @importFrom utils installed.packages
#' @title Add Elevation and Address Information to a Dataframe
#' @param df dataframe with geolocation information (\emph{e.g.} created by \code{wrcc_qualityControl()} or \code{airsis_qualityControl})
#' @param lonVar name of longitude variable in the incoming dataframe
#' @param latVar name of the latitude variable in the incoming dataframe
#' @param existingMeta existing 'meta' dataframe from which to obtain metadata for known monitor deployments
#' @description Google APIs are used to determine elevation and
#' address information associated with the locations specified by the
#' \code{longitude} and \code{latitude} columns of the incoming dataframe.
#' 
#' Address information is obtained by using the \pkg{ggmap} package.
#' @return Input dataframe with additional columns: \code{elevation, siteName, countyName}.
#' @references \url{https://developers.google.com/maps/documentation/elevation/intro}

addGoogleMetadata <- function(df, lonVar="longitude", latVar="latitude", existingMeta=NULL) {
  
  logger.warn("addGoogleMetadata() is deprecated. Please use addGoogleElevation() and addGoogleAddress() instead.")
  
  logger.trace(" ----- addGoogleMetadata() ----- ")
  
  # Sanity check -- make sure df does not have class "tbl_df" or "tibble"
  df <- as.data.frame(df, stringsAsFactors=FALSE)
  
  # Sanity check -- names
  if ( !lonVar %in% names(df) || !latVar %in% names(df) ) {
    logger.error("Dataframe does not contain columns lonVar='%s' or latVar='%s'", lonVar, latVar)
    logger.error("Please specify lonVar and latVar arguments")
    stop(paste0("Dataframe does not contain columns lonVar='",lonVar,"' or latVar='",latVar,"'"))
  }
  
  lons = df[[lonVar]]
  lats = df[[latVar]]
  
  # ----- Add elevation data (meters) from Google API ---------------------------
  
  if ( !is.null(existingMeta) ) {
    
    # NOTE:  If existingMeta is passed in, assume we are in an operational environment where we want to minimize google calls.
    
    for (i in 1:nrow(df)) {
      monitorID <- df[i,'monitorID']
      if ( monitorID %in% existingMeta$monitorID ) {
        df$elevation[i] <- existingMeta[monitorID,'elevation']
      } else {
        df$elevation[i] <- as.numeric(NA)
      }
    }
    
  } else {
    
    # NOTE:  No existingMeta so go ahead and query the google elevation service
    
    # Create url
    urlBase <- 'https://maps.googleapis.com/maps/api/elevation/json?locations='
    locations <- paste(lats, lons, sep=',', collapse='|')
    url <- paste0(urlBase, locations)
    
    # NOTE:  For now (2017-08-15) we aren't hitting Google limits because this service
    # NOTE:  accepts a vector of locations in a single web service call.
    
    logger.debug("Getting Google elevation data for %s location(s)", nrow(df))
    
    # Get and parse the return which has elements 'results' and 'status'
    response <- httr::GET(url)
    if ( httr::http_error(response) ) {
      stop(paste0("Google elevation service failed with: ",httr::content(response)))
    }
    
    googleReturn <- httr::content(response)
    
    # Check results
    if ( googleReturn$status != 'OK' ) {
      
      logger.warn("Google status was %s for URL %s", googleReturn$status, url)
      df$elevation <- as.numeric(NA)
      
    } else {
      
      # Convert list of lists to list of dataframes
      tempList <- lapply(googleReturn$results, as.data.frame, stringsAsFactors=FALSE)
      # Combine individual dataframes
      elevationDF <- dplyr::bind_rows(tempList)
      
      # Sanity check that things came back in the same order
      if ( !all(df[[latVar]] == elevationDF$location.lat) || !all(df[[lonVar]] == elevationDF$location.lon) ) {
        logger.error("Something is wrong with station elevation ordering")
        df$elevation <- as.numeric(NA)
      } else {
        df$elevation <- elevationDF$elevation
      }
      
    }
    
  } # end of !is.null(existingMetadata)
  
  # ----- Add siteName from Google API ---------------------------
  
  logger.debug("Getting site names for %s location(s)", nrow(df))
  
  # When siteName is missing, create one similar to AirNow with "locality-route"
  
  if ( !('siteName' %in% names(df)) ) df$siteName <- as.character(NA)
  if ( !('coutyName' %in% names(df)) ) df$countyName <- as.character(NA)
  
  # Use ggmap::revgeocode to return a dataframe with (address, street number, route, locality , ...)
  # (2500 queries allowed per day in August, 2015)
  if ('ggmap' %in% installed.packages()[,1]) {
    
    for (i in 1:nrow(df)) {
      
      # NOTE:  monitorID for AIRSIS and WRCC contains location information and will always
      # NOTE:  be associated with a unique siteName. Reusing metadata will dramatically 
      # NOTE:  decrease the number of Google API requests we make and will prevent null
      # NOTE:  responses when we are over our 2500 free requests.
      
      # Check for existing metadata for this monitorID
      metadataExists <- FALSE
      monitorID <- df[i,'monitorID']
      if ( !is.null(existingMeta) ) {
        if ( monitorID %in% existingMeta$monitorID ) {
          if ( !is.na(existingMeta[monitorID,'siteName']) && existingMeta[monitorID,'siteName'] != "" ) {
            metadataExists <- TRUE
          }
        }
      }
      
      if ( metadataExists ) {
        
        # Use existing siteName and countyName if the already exist
        logger.trace("\tusing existing metadata for %s", monitorID)
        df$siteName[i] <- existingMeta[monitorID,'siteName']
        df$countyName[i] <- existingMeta[monitorID, 'countyName']
        
      } else {
        
        # Query Google for siteName and countyName
        location <- c(df[i,lonVar],df[i,latVar])
        logger.trace("\tgoogle address request for location = %s, %s", location[1], location[2])
        if ( !anyNA(location) ) {
          address <- suppressMessages( ggmap::revgeocode(location, output='more') )
          # NOTE:  revgeocode can fail if you have too may Google requests in a day
          result <- try( df$siteName[i] <- paste(address$locality,address$route,sep='-'),
                         silent=TRUE ) # don't show errors
          if ( "try-error" %in% class(result) ) {
            logger.warn("Google geocoding may have failed. Have you gone over the 2.5K/day limit? (2017)")
          }
          # NOTE:  administrative_area_level_2 is not always present
          try( df$countyName[i] <- stringr::str_replace(address$administrative_area_level_2,' County',''),
               silent=TRUE ) # don't show errors
        }
        
      }
      
    }
    
  }
  
  return(df)
  
}
