#Shortcut script for (re)creating sysdata.rda
#

#IMPORTANT NOTE: spatial data doesn't behave well as internal package data. See the file data_load in the /R folder for a better way to do this. Non-spatial data can almost all be incorporated using  internal data though.

# DO NOT DO THE FOLLOWING FOR SPATIAL DATA:
# prov_buff <- sf::read_sf(dsn = "dev/prov_buffers", layer = "Provinces_buffered_300km")
#
#
# data <- list() #data must be a single object
#
# usethis::use_data(data, internal=TRUE, overwrite=TRUE)


# However, other data that can be simply reloaded as R environment objects can be made into internal data simply:
flow_returns <- read.csv("data-raw/flow_returns.csv")
level_returns <- read.csv("data-raw/level_returns.csv")
level_returns_max <- utils::read.csv("data-raw/level_returns_max.csv")
spatial_stns <- read.csv("data-raw/spatial_stns.csv")
peaks <- read.csv("data-raw/peaks.csv")
flow_level_flood <- read.csv("data-raw/flow_level_flood.csv")
snowcourse_factors <- read.csv("data-raw/snowcourse_factors.csv")
eq_std_calc_CCME_Mn <- read.csv("data-raw/eq_std_calc_CCME_Mn.csv")
eq_std_calc_CCME_NH4 <- read.csv("data-raw/eq_std_calc_CCME_NH4.csv")


data <- list(level_returns = level_returns, level_returns_max = level_returns_max, flow_returns = flow_returns, spatial_stns = spatial_stns, peaks = peaks, flow_level_flood = flow_level_flood, snowcourse_factors = snowcourse_factors, eq_std_calc_CCME_Mn = eq_std_calc_CCME_Mn, eq_std_calc_CCME_NH4 = eq_std_calc_CCME_NH4)

usethis::use_data(data, internal=TRUE, overwrite=TRUE)
