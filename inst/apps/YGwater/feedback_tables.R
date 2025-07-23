# This is a helper script to create tables necessary for the YGwater app to save user testing feedback. It assumes that the 'application' schema exists in the database, and that the logged user has the necessary privileges to create a new table in that schema.

# Connect to dev instance
con <- AquaConnect(username = "admin", password = "SnowFa11ing")

# Create temporary feedback table, used only for testing purposes
DBI::dbExecute(con, "CREATE TABLE application.feedback_temp (timestamp TIMESTAMP WITH TIME ZONE, username TEXT, bugs TEXT, ui_comment TEXT, module_comment TEXT, got_data BOOLEAN, missing TEXT)")
# Allow the public_reader role to read + write to the table
DBI::dbExecute(con, "GRANT SELECT, INSERT, UPDATE ON TABLE application.feedback TO public_reader")

# Create a new 'feedback' table with the permanent schema
DBI::dbExecute(con, "CREATE TABLE application.feedback (
               timestamp TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
               sentiment BOOLEAN,
               comment TEXT,
               page TEXT,
               app_state JSONB)")

# Give all roles append privilege to the new table
DBI::dbExecute(con, "GRANT INSERT ON TABLE application.feedback TO PUBLIC;")

DBI::dbDisconnect(con)



# Do the same thing for the prod database
con <- AquaConnect(host = "199.247.132.26", username = "admin", password = "SnowFa11ing")

# Create temporary feedback table, used only for testing purposes
DBI::dbExecute(con, "CREATE TABLE application.feedback_temp (timestamp TIMESTAMP WITH TIME ZONE, username TEXT, bugs TEXT, ui_comment TEXT, module_comment TEXT, got_data BOOLEAN, missing TEXT)")
# Allow the public_reader role to read + write to the table
DBI::dbExecute(con, "GRANT SELECT, INSERT, UPDATE ON TABLE application.feedback TO public_reader")

# Rename table 'feedback' to 'feedback_temp'
DBI::dbExecute(con, "ALTER TABLE application.feedback RENAME TO feedback_temp")
# Create a new 'feedback' table with the permanent schema
DBI::dbExecute(con, "CREATE TABLE application.feedback (
               timestamp TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
               sentiment BOOLEAN,
               comment TEXT,
               page TEXT,
               app_state JSONB)")

# Give all roles append privilege to the new table
DBI::dbExecute(con, "GRANT INSERT ON TABLE application.feedback TO PUBLIC;")

DBI::dbDisconnect(con)
