





library(exifr)

# Define the directory
directory <- "C:/Users/esniede/Documents/github/YGwater/dev/img_import_gui/data"

# Read all EXIF data from files in the directory
exif_data <- read_exif(list.files(directory, full.names = TRUE))

# View the EXIF data
print(exif_data)
