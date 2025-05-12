# LearnR Lesson 4

# Working with the WRB's database




# Note: skip this part if you don't have access to Yukon Government networks.
# Let's explore the AquaCache database a bit. We'll connect to it using a convenience function that's part of our main R package, YGwater (which we may need to install first)
# If you don't have YGwater installed yet, run the following line of code. If you get an error saying that 'remotes' is not installed, use install.packages("remotes") to install it.
remotes::install_github("YukonWRB/YGwater") # This line installs the YGwater package from GitHub, where our R pacakges are stored.

# Let's inspect the connection function, AquaConnect, to make sure we understand what's going on. First, the help file:
?YGwater::AquaConnect

# You'll see that each argument is pre-set to something, mostly objects refered to by 'Sys.getenv'. We'll get to those in a second.
# The line of code below will open the function's code in a new window:
View(YGwater::AquaConnect)
# Now find the bit of the function where the 'con' object is created, it looks something like con <- DBI::dbConnect(...). The function arguments are passed in to the dbConnect() function! The AquaConnect() function, then, just acts as a convenience wrapper around DBI::dbConnect(): it takes default arguments to make your life simpler, checks that you can actually get data, and gives you a reminder message.

# Now about those Sys.getenv calls. This tells R to look for environment variables which are already set and not necessarily visible in your Glocal Environment (top right pane in RStudio). We often use environment variables to store things like usernames and variables which can't be part of publicly shared code but which are nice for end users to 'set and forget'. You can set these variables in your .Renviron file, which is a hidden file in your home directory. You can find it by running the following line of code:
file.edit("~/.Renviron") # This opens the .Renviron file in a new window. If it doesn't exist, R will create it for you.

# You should add the following lines to your .Renviron file (remove the '#' symbols):
# aquacacheHost="10.250.12.154"
# aquacachePort="5432"
# aquacacheUser="public_reader"
# aquacachePass="aquacache"

# Restart R for the changes to take effect; meet back here in a few minutes.

# Now let's connect:
con <- YGwater::AquaConnect()

# (If that didn't work, double check your .Renviron file and if that checks out ask for help)

# Are you curious which tables are in the database? You can use the following line of code to get a list of all tables in the database:
DBI::dbListTables(con) # This line lists all tables in the database. 
# You can also use dbListFields(con, "table_name") to list all fields in a specific table.
DBI::dbListFields("locations") # This line lists all fields in the 'locations' table.

# If you're a bit familiar with the database structure, you'll remember that locations can have timeseries, and timeseries are unique on location, parameter, recording rate, and a few other things. Let's pull out a single entry from the locations table:
klondike <- DBI::dbGetQuery(con, "SELECT * FROM locations WHERE name = 'Klondike River above Bonanza Creek'") # dbGetQuery passes an SQL query to the database and returns the result as a data frame.
View(klondike) # This line opens the 'klondike' data frame in a new window.

# Cool! Now do you see the column called 'location_id'? This is a unique identifier for the location, and we can use it to get all timeseries for this location. Let's do that:
timeseries <- DBI::dbGetQuery(con, paste0("SELECT * FROM timeseries WHERE location_id = ", klondike$location_id))
View(timeseries) # This line opens the 'timeseries' data frame in a new window. You should see a few rows!
