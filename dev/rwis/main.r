### START SCRIPT ###

## hardcoded arguments
## collect arguments from command line
# stn_file <- args[4]       # locations for query

output_folder = 'dev/rwis'
# timezone <- "UTC"

## derived inputs (output file specification - note updated to make it a static name instead of date-based)
save_file <- paste0(output_folder, "/RWIS_measurements_FEWS.csv")
# save_file_meta <- paste0(output_folder, "/RWIS_metadata_FEWS.csv")

# Initialize save_file as an empty CSV with the expected columns
empty_df <- data.frame(
  station_id = character(),
  measurement_time = as.POSIXct(character()),
  ta = numeric(),
  tmax1 = numeric(),
  tmin1 = numeric(),
  swe = numeric(),
  sd = numeric(),
  rh = numeric(),
  precip_calc = numeric(),
  precip_rainonly_flag = logical()
)
utils::write.csv(empty_df, file = save_file, quote = FALSE, row.names = FALSE)

## support functions
# get subset of string from right side
# local function defined to avoid use of additional packages
substrLeft <- function(x, n){substr(x, 1,n)}
substrRight <- function(x, n){substr(x, nchar(x)-n+1, nchar(x))}
substrRmFirstLast <- function(x) {substr(x, 2, nchar(x)-1)}
massage_data <- function(x) {as.numeric(substrRmFirstLast(x))}


## start message thread for FEWS error messaging




start_date <- as.POSIXct("2020-01-01 00:00:00", tz = "UTC")
end_date <- as.POSIXct(Sys.time(), tz = "UTC")

important_params <- c("pcp1", "pcp6", "pcp12", "pcp24", "rn1")

important_headers <- c("station_id", "measurement_time")


RWIS <- YGwater::RWISConnect()
#on.exit(DBI::dbDisconnect(RWIS), add = TRUE)
# columns to read in
ss <- paste(c(important_params, important_headers), collapse = ", ")

start_date_UTC <- start_date
attr(start_date_UTC, "tzone") <- "UTC"
measurements <- DBI::dbGetQuery(RWIS, paste0("SELECT ", ss, " FROM measurements_measurement WHERE measurement_time > '", start_date_UTC, "' AND measurement_time <= '", .POSIXct(Sys.time(), tz = "UTC"), "';"))


# process data
## Remote the 'array' curly brackets from columns
measurements[ , important_params] <-
  suppressWarnings(apply(measurements[,important_params], MARGIN=c(1,2), FUN=massage_data))


# check if measurements is empty
    
    
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



rain <- measurements
rain <- rain[, c("measurement_time", "station_id", "rn1")]
# Remove rows where rn1 has more than one value or is NA
#rain <- rain[!is.na(rain$rn1) & lengths(regmatches(as.character(rain$rn1), gregexpr("[^,]+", as.character(rain$rn1))) ) == 1, ]

# Parse rn1 by comma and take the left value, then remove curly brackets
rain$rn1 <- gsub("[{}]", "", rain$rn1)
rain$rn1 <- sapply(strsplit(as.character(rain$rn1), ","), function(x) x[1])
rain$rn1 <- as.numeric(rain$rn1)


rain <- na.omit(rain)
rain_wide <- reshape(
  rain,
  idvar = "measurement_time",
  timevar = "station_id",
  direction = "wide"
)
rownames(rain_wide) <- rain_wide$measurement_time
rain_wide$measurement_time <- NULL
rain_wide <- rain_wide[order(as.POSIXct(rownames(rain_wide))), ]
plot(
  as.POSIXct(rownames(rain_wide)),
  rain_wide[["rn1.87"]],
  type = "l",
  xlab = "Time",
  ylab = "Rainfall (rn1) at Station 87",
  main = "Rainfall at Station 87"
)



# Download stations_stations table from RWIS
RWIS <- YGwater::RWISConnect()
writeLines(sprintf("RWIS connection established"), fc)
on.exit(DBI::dbDisconnect(RWIS), add = TRUE)

writeLines(sprintf("Reading stations_stations data from RWIS..."), fc)
stations <- DBI::dbGetQuery(RWIS, "SELECT * FROM stations_station;")

colnames(rain_wide) <- sub("^rn1\\.", "", colnames(rain_wide))
station_map <- setNames(stations$abbreviation, stations$id)
colnames(rain_wide) <- station_map[colnames(rain_wide)]


rain_wide


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
