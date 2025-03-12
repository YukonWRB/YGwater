


library(YGwater)
library(ncdf4)

important_params <- c("ta", "tmax1", "tmin1", "pcp1", "pcp6", "pcp12", "pcp24", "rn1", "swe", "sd", "rh")
important_headers <- c("station_id", "measurement_time")

## download RWIS data from local database as first step
## splitting retrieval and filter/writing into two steps due to size of database and possibility of errors
result <- tryCatch({
    writeLines(sprintf("INFO - Connecting to RWIS"), fc)
      RWIS <- YGwater::RWISConnect()
      writeLines(sprintf("RWIS connection established"), fc)
      on.exit(DBI::dbDisconnect(RWIS), add = TRUE)

      # columns to read in
      ss <- paste(c(important_params, important_headers), collapse = ", ")
      
      # read in data
      writeLines(sprintf("Reading data from RWIS..."), fc)
      start_date_UTC <- start_date
      attr(start_date_UTC, "tzone") <- "UTC"
      measurements <- DBI::dbGetQuery(RWIS, paste0("SELECT ", ss, " FROM measurements_measurement WHERE measurement_time > '", start_date_UTC, "' AND measurement_time <= '", .POSIXct(Sys.time(), tz = "UTC"), "';"))
      # process data
      ## Remote the 'array' curly brackets from columns
      measurements[ , important_params] <-
        suppressWarnings(apply(measurements[,important_params], MARGIN=c(1,2), FUN=massage_data))
      writeLines(sprintf("RWIS data fetched"), fc)
  },
  error = function(cond) {
    message(cond)
    writeLines(sprintf("ERROR - RWIS data %s NOT retrieved successfully: %s", as.character(cond)), fc)
  },
  warning = function(cond) {
    message(cond)
    writeLines(sprintf("WARNING - Possible issue(s) retrieving RWIS data from local database: %s", as.character(cond)), fc)
  })

if (nrow(measurements) == 0) {
  writeLines(sprintf("ERROR - No data was fetched. Check time bounds for filtering."), fc)
  stop()
}

result <- tryCatch({
    writeLines(sprintf("DEBUG - processing RWIS data"), fc)
    # measurements is directly overwritten to avoid holding large RWIS data set in memory for too long
    
    # filter stations
    ## skipped location filtering in this script
    # measurements <-
    #   measurements[measurements$id %in% stn_list$stn_id,]

    
    ### process precipitation data
    # We need precip in FEWS to be hourly. However, sometimes pcp is recorded at 6, 12, or 24 hours intervals. We'll take those lengthier integration periods and split them up into 1 hour chunks.
    measurements$precip_calc <- measurements$pcp1
    measurements$precip_rainonly_flag <- FALSE
    
    for (i in 1:nrow(measurements)) {
      # check if precip based on hourly precip is NA
      if (is.na(measurements$precip_calc[i])) {
        # look for 6 hour precip
        temp <- measurements[measurements$station_id == measurements$station_id[i] &
                               measurements$measurement_time >= measurements$measurement_time[i] &
                               measurements$measurement_time < measurements$measurement_time[i] + 6*60*60 &
                               !is.na(measurements$pcp6), ]
        if (nrow(temp) == 0) {
          # look for 12 hour precip
          temp <- measurements[measurements$station_id == measurements$station_id[i] &
                                 measurements$measurement_time >= measurements$measurement_time[i] &
                                 measurements$measurement_time < measurements$measurement_time[i] + 12*60*60 &
                                 !is.na(measurements$pcp12), ]
          if (nrow(temp) == 0) {
            # look for 24 hour precip
            temp <- measurements[measurements$station_id == measurements$station_id[i] &
                                   measurements$measurement_time >= measurements$measurement_time[i] &
                                   measurements$measurement_time < measurements$measurement_time[i] + 24*60*60 &
                                   !is.na(measurements$pcp24), ]
            if (nrow(temp) == 0) {
              # look for rainfall
              if (!is.na(measurements$rn1[i])) {
                measurements$precip_calc[i] <- measurements$rn1[i]
                measurements$precip_rainonly_flag[i] <- TRUE
              }
            } else { # use 24 hour precip
              datetimes <- seq.POSIXt(from =  measurements$measurement_time[i] - 23*60*60, to = measurements$measurement_time[i], by = "hour")
              avg <- mean(temp$pcp24)/24
              measurements[measurements$station_id == measurements$station_id[i] & measurements$measurement_time %in% datetimes, "precip_calc"] <- avg
            }
          } else { # use 12 hour precip
            datetimes <- seq.POSIXt(from =  measurements$measurement_time[i] - 11*60*60, to = measurements$measurement_time[i], by = "hour")
            avg <- mean(temp$pcp24)/12
            measurements[measurements$station_id == measurements$station_id[i] & measurements$measurement_time %in% datetimes, "precip_calc"] <- avg
          }
        } else { # use 6 hour precip
          datetimes <- seq.POSIXt(from =  measurements$measurement_time[i] - 5*60*60, to = measurements$measurement_time[i], by = "hour")
          avg <- mean(temp$pcp24)/6
          measurements[measurements$station_id == measurements$station_id[i] & measurements$measurement_time %in% datetimes, "precip_calc"] <- avg
        }
      } # else, we have our hourly precip value and we are done
    }
    
    # Drop redundant precip columns
    measurements <- measurements[ , -which(names(measurements) %in% c("pcp1", "pcp6", "pcp12", "pcp24", "rn1"))]

    writeLines("INFO - Finished processing RWIS data", fc)

  },
  error = function(cond) {
    message(cond)
    writeLines(sprintf("ERROR - RWIS data processsing NOT done successfully: %s",as.character(cond)), fc)
  },
  warning = function(cond) {
    message(cond)
    writeLines(sprintf("WARNING - Possible issue(s) processing RWIS data: %s",as.character(cond)), fc)
  }
)

writeLines(sprintf("DEBUG - writing RWIS data to local files"), fc)

## write RWIS data to file
result <- tryCatch(
  {
    ## not writing metadata currently, but could write metadata from RWIS also
    utils::write.csv(measurements, file = save_file,
              quote = FALSE, row.names = FALSE)

    writeLines(sprintf("INFO - Finished writing RWIS data with %i rows to local file: %s", nrow(measurements), save_file), fc)

  },
  error = function(cond) {
    message(cond)
    writeLines(sprintf("ERROR - RWIS data writing NOT done successfully: %s",as.character(cond)), fc)
  },
  warning = function(cond) {
    message(cond)
    writeLines(sprintf("WARNING - Possible issue(s) writing RWIS data to file %s:\n%s",
                       save_file,as.character(cond)), fc)
  }
)

## wrap up
writeLines(sprintf("INFO - DownloadRWISData Rscript finished"), fc)
close(fc)
