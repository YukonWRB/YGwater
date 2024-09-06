#' Get flow rating curves from Aquarius
#'
#'@description
#' `r lifecycle::badge("experimental")`
#'
#' Fetches and processes rating curve data downloaded from an Aquarius web-hosted server and returns it in a concise format. Information about all rating curves for a location can be returned, or the user can specify a particular input and output parameter for which to retrieve the rating curve. This function also requests input-output relationships from Aquarius and fits a power law or polynomial model to the data; the best-fit model is selected based on AIC and the equation returned. 
#'
#'@details
#' To store login credentials in your .renviron file, call [usethis::edit_r_environ()] and enter your username and password as value pairs, as AQUSER="your username" and AQPASS="your password". The server should be entered at server="your_server_url".
#'
#'
#' @param loc_id The location ID, exactly as visible in Aquarius web portal, as a character vector of length 1. Typically of form `29EA001` or `YOWN-0804`. Note that the timeseries id is not used, only the location ID.
#' @param inputParameter The input parameter for which you want to know the rating curve. If NULL, all rating curves for the location will be returned. This should match the 'Parameter' in Aquarius exactly.
#' @param outputParameter The output parameter for which you want to know the rating curve. If NULL, all rating curves for the location will be returned. This should match the 'Parameter' in Aquarius exactly.
#' @param start The first day or instant for which you want information, in UTC 0 timezone. You can specify a Date object, POSIXct object, or character vector of form yyyy-mm-dd or yyyy-mm-dd HH:mm:ss. If specifying date or POSIXct objects, the timezone attribute will be ignored. If only a date is specified it will be assigned the first moment of the day. Times requested prior to the actual timeseries start will be adjusted to match available data.
#' @param end The last day or instant for which you want information, in UTC 0. You can specify a Date object, POSIXct object, or character vector of form yyyy-mm-dd or yyyy-mm-dd HH:mm:ss. If specifying date or POSIXct objects, the timezone attribute will be ignored. If only a date is specified it will be assigned the last moment of the day. Times requested prior to the actual timeseries end will be adjusted to match available data.
#' @param login Your Aquarius login credentials as a character vector of two. Default pulls information from your .renviron file; see details.
#' @param server The URL for your organization's Aquarius web server. Default pulls from your .renviron file; see details.
#'
#' @return A list with two data.frames: station metadata (simple data.frame) and rating curve information (complex data.frame). The later contains one row per parameter which has a rating curve (i.e. a distinct curve ID), with the final column containing a list of all the curves associated with that curve ID.
#'
#' @export

aq_download_curves <- function(loc_id,
                               inputParameter = NULL,
                               outputParameter = NULL,
                               start = "1950-01-01",
                               end = Sys.Date(),
                               login = Sys.getenv(c("AQUSER", "AQPASS")),
                               server = Sys.getenv("AQSERVER")
)
{
  
  # loc_id <- "29AB007"
  # start <- "2000-01-01"
  # end <- Sys.Date()
  # login <- Sys.getenv(c("AQUSER", "AQPASS"))
  # server <- Sys.getenv("AQSERVER")
  # inputParameter <- NULL
  # outputParameter <- NULL

  source(system.file("scripts",  "timeseries_client.R", package = "YGwater")) #This loads the code dependencies (though some have been put directly in this function for portability and to fix errors)
  
  
  # Taken from timeseries_client and adapted (debugged)  (in /inst)
  formatIso8601 <- function(datetime) {
    isoText <- strftime(datetime, "%Y-%m-%dT%H:%M:%OS%z", "UTC")
    
    len <- nchar(isoText)
    
    if (substr(isoText, len, len) != "Z") {
      # Inject the missing colon in the zone offset, so "+HHMM" becomes "+HH:MM"
      isoText = paste0(substr(isoText, 1, len - 2), ":", substr(isoText, len - 1, len))
    }
    
    isoText
  }

  publishUri <- paste0(server, "/Publish/v2")
  
  # Taken from timeseries_client and adapted (debugged)  (in /inst)
  sendBatchRequests <- function(endpoint, operationName, operationRoute, requests, batchSize, verb) {
    
    if (missing(batchSize)) { batchSize <- 100 }
    if (missing(verb))      { verb <- "GET" }
    
    if (batchSize > 500)    { batchSize <- 500 }

      # Compose the special batch-operation URL supported by ServiceStack
      url = paste0(endpoint, "/json/reply/", operationName, "[]")
      
      # Split the requests into batch-sized chunks
      requestBatches <- split(requests, ceiling(seq_along(requests) / batchSize))
      
      # Create a local function to request each batch of requests, using the verb as an override to the POST
      batchPost <- function(batchOfRequests, index) {
        offset <- batchSize * index
        r <- httr::POST(url, body = batchOfRequests, encode = "json", httr::add_headers("X-Http-Method-Override" = verb))
        httr::stop_for_status(r, paste("receive", length(batchOfRequests), "batch", operationRoute, "responses at offset", offset))
        
        # Return the batch of responses
        responses <- jsonlite::fromJSON(httr::content(r, "text"))
      }
      
      # Call the operation in batches
      responseBatches <- mapply(batchPost, requestBatches, seq_along(requestBatches) - 1, SIMPLIFY = FALSE)
      
      # Flatten the list of response data frames into a single data frame
      jsonlite::rbind_pages(responseBatches)
    }
  
  # Taken from timeseries_client and adapted (debugged)  (in /inst)
  # Gets the rating models matching the optional filters
  getRatings <- function(locationIdentifier, queryFrom, queryTo, inputParameter, outputParameter) {
    
    if (missing(locationIdentifier))  { locationIdentifier = NULL }
    if (missing(inputParameter))      { inputParameter = NULL }
    if (missing(outputParameter))     { outputParameter = NULL }
    if (missing(queryFrom))           { queryFrom = NULL }
    if (missing(queryTo))             { queryTo = NULL }
    
    # Coerce native R dates to an ISO 8601 string
    if (is.double(queryFrom)) { queryFrom <- formatIso8601(queryFrom) }
    if (is.double(queryTo))   { queryTo   <- formatIso8601(queryTo) }
    
    # Build the rating model query
    q <- list(
      LocationIdentifier = locationIdentifier,
      InputParameter = inputParameter,
      OutputParameter = outputParameter,
      QueryFrom = queryFrom,
      QueryTo = queryTo)
    q <- q[!sapply(q, is.null)]
    
    # Get the rating models for the time period
    ratingModels <- jsonlite::fromJSON(httr::content(httr::stop_for_status(
      httr::GET(paste0(publishUri, "/GetRatingModelDescriptionList"), query = q)
      , paste("get rating models for", locationIdentifier)), "text"))$RatingModelDescriptions
    
    # Get the rating curves active in those models
    ratingCurveRequests <- lapply(ratingModels$Identifier, function(identifier) {
      r <- list(
        RatingModelIdentifier = identifier,
        QueryFrom = queryFrom,
        QueryTo = queryTo)
      r <- r[!sapply(r, is.null)]
    })
    

    ratingCurves <- sendBatchRequests(publishUri, "RatingCurveListServiceRequest", "/GetRatingCurveList", ratingCurveRequests)
    ratingModels$Curves <- ratingCurves$RatingCurves
    
    ratingModels
  }
  
  # Gets output values from a rating model. This is used to build input:output relationships and to calculate a best fit equation
  #
  # @param ratingModelIdentifier The identifier of the rating model
  # @param inputValues The list of input values to run through the model
  # @param effectiveTime Optional time of applicability. Assumes current time if omitted
  # @param applyShifts Optional boolean, defaults to FALSE
  # @return The output values from the applicable curve of the rating model. An output value of NA is returned if the input is outside the curve.
  getRatingModelOutputValues <- function(ratingModelIdentifier, inputValues, effectiveTime, applyShifts) {
    
    if (missing(effectiveTime)) { effectiveTime <- NULL }
    if (missing(applyShifts))   { applyShifts <- NULL }
    
    # Coerce native R dates to an ISO 8601 string
    if (is.double(effectiveTime)) { effectiveTime <- formatIso8601(effectiveTime) }
    
    # Build the query
    q <- list(
      RatingModelIdentifier = ratingModelIdentifier,
      InputValues = timeseries$toJSV(inputValues),
      EffectiveTime = effectiveTime,
      ApplyShifts = applyShifts
    )
    q <- q[!sapply(q, is.null)]
    
    # Get the output values of the rating curve
    outputValues <- jsonlite::fromJSON(httr::content(httr::stop_for_status(
      httr::GET(paste0(publishUri, "/GetRatingModelOutputValues"), query = q)
      , paste("get rating model output values for", ratingModelIdentifier)), "text"))$OutputValues
    
  }
  
  
  # Now that the functions are defined, connect to AQ and get the data out
  
  #Make the Aquarius configuration
  config = list(
    server = server,
    username = login[1],
    password = login[2]
  )
  
  # Connect to Aquarius server
  timeseries$connect(config$server,
                     config$username,
                     config$password)
  on.exit(timeseries$disconnect())
  
  # Get the location metadata
  locationData = timeseries$getLocationData(loc_id)
  
  # Get info about the rating models for the location over the time period specified
  ratingModels <- getRatings(locationIdentifier = loc_id,
                             queryFrom = start,
                             queryTo = end,
                             inputParameter = inputParameter,
                             outputParameter = outputParameter)
  
  ratingModelsList <- list()
  for (i in 1:nrow(ratingModels)) {
    curves <- ratingModels$Curves[[i]]
    list_element_name <- ratingModels$Identifier
    
    # Iterate over each curve to get the relationship between input and output
    curve_details <- list()
    for (j in 1:nrow(curves)) {
      curve <- curves[j, ]
      curve_name <- curve$Id
      skip <- FALSE
      tryCatch({
        range <- curve$BaseRatingTable[[1]]$InputValue
        stage <- seq(round(min(range), digits = 2), round(max(range), digits = 2), by = 0.01)
        discharge <- getRatingModelOutputValues(ratingModels$Identifier[[i]], stage, effectiveTime = curve$PeriodsOfApplicability[[1]][1])
      }, error = function(e) {
        ratingModelsList[[list_element_name]][[curve_name]] <<- list(curve, curve_details = "Curve details could not be retrieved for this curve (likely failed to extract relationship info from Aquarius).")
        skip <<- TRUE
      })
      if (skip) next
      
      # Make a data.frame to return to the user
      relationship <- data.frame(stage = stage, discharge = discharge)
      names(relationship) <- c(paste0(ratingModels$InputParameter[[i]], " (", ratingModels$InputUnit[[i]], ")"), paste0(ratingModels$OutputParameter[[i]], " (", ratingModels$OutputUnit[[i]], ")"))
      
      tryCatch({
        # Removing NA values for fitting
        valid_data <- data.frame(stage = stage[!is.na(discharge)], discharge = discharge[!is.na(discharge)])
        
        # Power Law Fitting
        no_power_law <- FALSE
        tryCatch({
          power_law <- nls(discharge ~ a * (stage - h0)^b, data = valid_data, start = list(a = 1, h0 = min(valid_data$stage), b = 1))
        }, error = function(e) {
          power_law <<- NULL
          no_power_law <<- TRUE
        })
        # Polynomial fitting
        no_poly <- FALSE
        tryCatch({
          poly <- lm(discharge ~ poly(stage, 2), data = valid_data)
        }, error = function(e) {
          poly <<- NULL
          no_poly <<- TRUE
        })
        # Calculate AIC and select the best performing model
        if (no_power_law || no_poly) {
          if (no_power_law && no_poly) {
            stop("No model could be fit to the data.")
          } else if (no_power_law) {
            best_model <- "poly"
          } else {
            best_model <- "power_law"
          }
        } else {
          AIC <- c(AIC(power_law), AIC(poly))
          best_model <- ifelse(AIC[1] == min(AIC), "power_law", "poly")
        }
        
        
        # Extract the equation based on the best model
        if (best_model == "power_law") {
          # Extract coefficients from the power law model
          coef_vals <- coef(power_law)
          equation <- paste0("output = ", round(coef_vals['a'], 4), " * (input - ", round(coef_vals['h0'], 4), ")^", round(coef_vals['b'], 4))
        } else {
          # Extract coefficients from the polynomial model
          coef_vals <- coef(poly)
          equation <- paste0("output = ", round(coef_vals[1], 4), " + ", round(coef_vals[2], 4), " * input + ", round(coef_vals[3], 4), " * input^2")
        }
      }, error = function(e) {
        equation <- "No equation could be fit to the data."
      })
      
      curve_details[[curve_name]] <- list(curve = curve, 
                                          relationship = relationship, 
                                          equation = equation)
    }
    ratingModelsList[[list_element_name]] <- curve_details
  }
  
  metadata <- data.frame(attribute = c("Location Name", "Identifier", "Location Type", "Latitude", "Longitude", "Elevation", "Elevation Units", "UTC Offset in Aquarius"),
                         value = c(locationData$LocationName, locationData$Identifier, locationData$LocationType, locationData$Latitude, locationData$Longitude, locationData$Elevation, if (is.null(locationData$ElevationUnits)) "unspecified" else locationData$ElevationUnits, locationData$UtcOffset)
  )
  list <- list(metadata = metadata,
               ratingModelsList = ratingModelsList)
  
  return(list)
  
}
