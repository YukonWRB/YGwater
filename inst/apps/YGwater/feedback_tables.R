# This is a helper script to create tables necessary for the YGwater app to save user testing feedback. It assumes that the 'application' schema exists in the database, and that the logged user has the necessary privileges to create a new table in that schema.

DBI::dbExecute(con, "CREATE TABLE application.feedback (timestamp TIMESTAMP WITH TIME ZONE, username TEXT, bugs TEXT, ui_comment TEXT, module_comment TEXT, got_data BOOLEAN, missing TEXT)")

# Allow the public_reader role to read + write to the table
DBI::dbExecute(con, "GRANT SELECT, INSERT, UPDATE ON TABLE application.feedback TO public_reader")
