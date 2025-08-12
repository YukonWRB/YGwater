# This is a helper script to create tables necessary for the YGwater app to save user testing feedback. It assumes that the 'application' schema exists in the database, and that the logged user has the necessary privileges to create a new table in that schema.

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
