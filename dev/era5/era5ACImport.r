# You must make a free account with Copernicus to obtain the details below:

library(ecmwfr)

wf_set_key(key = "5815cfa9-2642-46bd-9a7f-9ac2099b32f4")
data_dir = "dev/era5/.data/rraw"

download_era5 <- function(
    data_dir,
    start_date,
    end_date,
    frequency = 6,
    latency = 5
) {

    # make sure the frequency is a factor of 24
    if (24 %% frequency != 0) {
        stop("Frequency must be a factor of 24.")
    }

    # default end date as current date minus latency days
    if (is.na(end_date)) {
        end_date <- format(Sys.time(), "%Y-%m-%d %H:%M")
        end_date <- as.POSIXct(end_date, tz = "UTC") - as.difftime(latency, units = "days")
    }

    # check if end date is at least 5 days prior to current date
    if (end_date > Sys.time() - as.difftime(5, units = "days")) {
        stop("end_date must be at least 5 days prior to today's date.")
    }

    datetime_array <- seq(
        from = as.POSIXct(start_date, tz = "UTC"),
        to = as.POSIXct(end_date, tz = "UTC"),
        by = paste(frequency, "hours")
    )

    for (ii in seq_along(datetime_array)) {
        datetime <- datetime_array[ii]
        year <- sprintf("%d", as.numeric(format(datetime, "%Y")))
        month <- sprintf("%02d", as.numeric(format(datetime, "%m")))
        day <- sprintf("%02d", as.numeric(format(datetime, "%d")))
        hour <- sprintf("%02d", as.numeric(format(datetime, "%H")))

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
            area = c(72, -150, 55, -120),
            target = paste0("ERA5_data_", year, month, day, hour, ".nc")
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
    }
}

start_date <- "2020-01-01 00:00"
end_date <- "2020-05-01 00:00"
download_era5(data_dir = data_dir, start_date, end_date, frequency = 6, latency = 5)


# Unzip the downloaded file
unzip("ERA5_data_2020010100.zip", exdir = "./unzipped_data")

# Rename the unzipped file
unzipped_files <- list.files("./unzipped_data", full.names = TRUE)
original_zip_files <- list.files(pattern = "\\.zip$")

for (i in seq_along(unzipped_files)) {
    new_name <- sub("\\.zip$", ".nc", original_zip_files[i])
    file.rename(unzipped_files[i], file.path("./unzipped_data", new_name))
}