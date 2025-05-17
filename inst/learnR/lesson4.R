# LearnR Lesson 4

# Working with the WRB's database




# Note: skip this lesson if you don't have access to Yukon Government networks.

# Let's explore the AquaCache database a bit. We'll connect to it using a convenience function that's of this R package, YGwater.

# We'll inspect the connection function, AquaConnect, to make sure we understand what's going on. First, the help file:
?YGwater::AquaConnect

# You'll see that each argument is pre-set to something, mostly objects referred to by 'Sys.getenv(variableName)'. We'll get to those in a second.
# The line of code below will open the function's code in a new window:
View(YGwater::AquaConnect)

# Now find the bit of the function where the 'con' object is created, it looks something like con <- DBI::dbConnect(...). The function arguments are passed in to the dbConnect() function! The AquaConnect() function, then, just acts as a convenience wrapper around DBI::dbConnect(): it takes default arguments to make your life simpler, checks that you can actually get data, and gives you a reminder message.

# Now about those Sys.getenv calls. This tells R to look for environment variables which are already set and not visible in your Global Environment (top right pane in RStudio). We often use these types of variables to store things like usernames and variables which can't be part of publicly shared code but which are nice for end users to 'set and forget'. You can set these in your .Renviron file, which is a hidden file in your home directory. You can find it by running the following line of code:
file.edit("~/.Renviron") # This opens the .Renviron file in a new window. If it doesn't exist, R will create it for you.

# You should add the following lines to your .Renviron file if they're not there yet (remove the '#' symbols):
# aquacacheHost="10.250.12.154"
# aquacachePort="5432"
# aquacacheUser="public_reader"
# aquacachePass="aquacache"

# Restart R for the changes to take effect; meet back here in a few minutes.

# Now let's connect to the database. You can do this by running the following line of code:
con <- YGwater::AquaConnect()

# (If that didn't work, double check your .Renviron file and if that checks out ask for help)

# Let's explore the database a bit now.
# Are you curious which tables are in the database? You can use the following line to list all tables in the database:
DBI::dbListTables(con) # This line lists all tables in the database. 

# You can also use dbListFields(con, "table_name") to list all fields in a specific table.
DBI::dbListFields(con, "locations") # This line lists all fields in the 'locations' table.

# If you're a bit familiar with the database structure, you'll remember that locations can have timeseries, and timeseries are unique on location, parameter, recording rate, and a few other things. Let's extract all records from the 'locations' table and take a peek:
locations <- DBI::dbGetQuery(con, "SELECT * FROM locations") # dbGetQuery passes an SQL query to the database and returns the result as a data frame.
View(locations) # This line opens the 'locations' data frame in a new window.

# A lot of the columns extracted are not very useful for us. The line below extracts only the column names:
colnames(locations) # This line lists all column names in the 'locations' data frame.
# We have a few options to get only the columns we want:
# 1. Working with the data.frame, we can drop unnecessary columns one by one:
locations$data_sharing_agreement_id <- NULL
# Use View(locations) or head(locations) to check that the column was removed if you need proof!
# 2. We can select only the columns we want, and re-order at the same time:
locations <- locations[ , c("location_id", "location", "name", "latitude", "longitude")]  # Reminder about data.frame subsetting notation: df[row, column]!!! 
# It **is** possible to subset by numeric index instead of column name, but it's not recommended because it makes your code less readable and can't adapt to changes in the data structure.
# .... and....
# We can use a better database query!
locations <- DBI::dbGetQuery(con, "SELECT location_id, location, name, latitude, longitude FROM locations") # This line extracts only the columns we want from the database.
head(locations) # This lets us take a look at the first few rows of the data frame. You can also use View(locations) to open the data frame in a new window.

# Cool! Now do you see the column called 'location_id'? This is a unique identifier for the location, and we can use it to get all timeseries for this location from table 'timeseries', and later to get the actual timeseries data. Let's do that:
# Take a look at the 'locations' data.frame and find a location you like. For example, the Tagish Meteorological one is a good one to use and has fun snow pillow data to plot. Now note down the location_id for that location. You can also use the following line to get the location_id for the Tagish Meteorological location:
tagish <- locations[locations$name == "Tagish Meteorological", "location_id"] # This line extracts the row of the 'locations' data frame where the location is 'Tagish Meteorological'.

# Now let's get the timeseries for that location. You can do this by running the following line of code:
tagish_timeseries <- DBI::dbGetQuery(con, paste0("SELECT * FROM timeseries WHERE location_id = ", tagish))
View(tagish_timeseries) # This line opens the 'timeseries' data frame in a new window. You should see a few rows!


# Now let's pull the timeseries data for SWE at Tagish. The problem is, though, which timeseries is the one for SWE? The column 'parameter_id' could tell us that, but we don't know what the numbers mean! In databases, we use foreign keys to link id columns like that to their parent records; this prevents unnecessary re-writing of data and keeps only one authoritative record. We can use SQL to find foreign keys, but it's a bit of a pain... much easier to see that using a tool like DBeaver. I'll walk you though this one for now.
# The referenced table is table 'parameters'. Let's see it here. Note that I'm not creating an object in the environment for this one (I'm not using the <- operator):
View(DBI::dbGetQuery(con, "SELECT * FROM parameters")) # This line extracts all columns from the 'parameters' table.

# Well wouldn't you know it, there's a column called 'parameter_id'!!! Now we can find the right parameter_id for SWE by scrolling through the table, since it might not be named exactly SWE. Take a look and come back with the parameter_id for SWE.

# ...

# You'll have noticed that 'SWE' is actually 'snow water equivalent' in the database, and that the parameter_id is 21. Now look again at the 'tagish_timeseries' object you created earlier to find the timeseries_id you need. You shouldn't need my help for that anymore!

# ...

# Ok, now you have a timeseries_id! Let's get the data for the timeseries. First, you'll have to know which table to look at.... and it's in fact not a table at all: it's a view! Database views are essentially tables created on-demand from existing table data, possibly with calculations or other transformation in the pipeline. In this case we're querying the view and not the table because corrections are applied to the view if necessary, and because the visibility of some rows is restricted at the view level.
# Here's the SQL to get the data, but you'll need to replace the ??? with the timeseries_id of interest:
data <- DBI::dbGetQuery(con, paste0("SELECT * FROM measurements_continuous_corrected WHERE timeseries_id = ???"))

# Now let's get an understanding of the data. I find it most useful to know the number of rows (nrow()) as well as what the first few rows looks like (head()). Run those commands now so we're on the same page.

# ...

# Awesome! 
