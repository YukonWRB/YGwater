# You must make a free account with Copernicus to obtain the details below:

library(ecmwfr)

wf_set_key(key = "5815cfa9-2642-46bd-9a7f-9ac2099b32f4")

data_dir = "dev/era5/.data/rraw"

downloadERA5 <- function(
    data_dir=tempdir(),
    start_date,
    end_date = NA,
    num_days = NA,
    frequency = 6,
    latency = 5,
    area = c(72, -150, 55, -120),
    ) {

    # make sure the frequency is a factor of 24
    if (24 %% frequency != 0) {
        stop("Frequency must be a factor of 24 (e.g. 24 will download 00:00, 6 will download 00:00, 06:00, 12:00, 18:00)")
    }


    # default end date as current date minus latency days
    if (is.na(end_date)) {
        end_date <- format(Sys.time(), "%Y-%m-%d %H:%M")
        end_date <- as.POSIXct(end_date, tz = "UTC") - as.difftime(latency, units = "days")
    }

    # check if end date is at least 5 days prior to current date
    if (end_date > Sys.time() - as.difftime(5, units = "days")) {
        warning("end_date must be at least 5 days prior to today's date.")
        end_date <- format(Sys.time(), "%Y-%m-%d %H:%M")
        end_date <- as.POSIXct(end_date, tz = "UTC") - as.difftime(latency, units = "days")
        #stop("end_date must be at least 5 days prior to today's date.")
    }


    if (is.na(num_days) & is.na(start_date)) {
        stop("Either num_days or start_date must be provided.")
    } else if (!is.na(num_days) & is.na(start_date)) {
        start_date = end_date - as.difftime(num_days, units = "days")


    datetime_array <- seq(
        from = as.POSIXct(start_date, tz = "UTC"),
        to = as.POSIXct(end_date, tz = "UTC"),
        by = paste(frequency, "hours")
    )



    files = list()
    
    for (ii in seq_along(datetime_array)) {
        datetime <- datetime_array[ii]
        year <- sprintf("%d", as.numeric(format(datetime, "%Y")))
        month <- sprintf("%02d", as.numeric(format(datetime, "%m")))
        day <- sprintf("%02d", as.numeric(format(datetime, "%d")))
        hour <- sprintf("%02d", as.numeric(format(datetime, "%H")))
        name <- paste0("ERA5_swe_", year, month, day, hour, ".nc")
        # Create the request for the ERA5 data

        request <- list(
            dataset_short_name = "reanalysis-era5-land",
            product_type = "reanalysis",
            variable = "snow_depth_water_equivalent",
            year = year,
            month = month,
            day = day,
            time = paste0(hour, ":00"),
            format = "netcdf",
            area = area,
            target = name
        )

        # Download the data using the Copernicus API
        # Note: You need to have the Copernicus API credentials set up in your .Renviron file
        # or use the `wfr::set_key()` function to set them in your R session
        wf_request(
            request = request,  # the request
            transfer = TRUE,
            path = data_dir,
            user = "everett.snieder@gmail.com"
        )

        # Construct the full path for the zip file
        zip_file <- file.path(data_dir, name)
        unzip(zip_file, exdir = data_dir)
        file.rename(paste0(data_dir, "/data_0.nc"), paste(data_dir, "/", basename(zip_file), ".nc", sep = ""))
        
        # Delete the zip file after processing
        file.remove(zip_file)


    } # end datetime iteration
}
}
    

start_date <- "2025-04-01 00:00"
download_era5(data_dir = data_dir, start_date, frequency = 6, latency = 5)


# Unzip all files in "dev/era5/.data/rraw" to "dev/era5/.data/runzip"
raw_dir <- "dev/era5/.data/rraw"
unzip_dir <- "dev/era5/.data/rextracted"

# Create the unzip directory if it doesn't exist
if (!dir.exists(unzip_dir)) {
    dir.create(unzip_dir)
}

# List all zip files in the raw directory
zip_files <- list.files(raw_dir, pattern = "\\.zip$", full.names = TRUE)

# Unzip each file into the unzip directory
for (zip_file in zip_files) {
    unzip(zip_file, exdir = unzip_dir)
    file.rename(paste0(unzip_dir, "/data_0.nc"), paste(unzip_dir, "/", basename(zip_file), ".nc", sep = ""))
}


library(terra)

# List all .nc files in the unzip directory
nc_files <- list.files(unzip_dir, pattern = "\\.nc$", full.names = TRUE)

# Open the first .nc file as a terra rast object
if (length(nc_files) > 0) {
    first_rast <- rast(nc_files[1])
    print(first_rast)
} else {
    stop("No .nc files found in the unzip directory.")
}