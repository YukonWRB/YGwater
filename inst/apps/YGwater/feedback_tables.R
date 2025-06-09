# This is a helper script to create tables necessary for the YGwater app to save user testing feedback. It assumes that the 'application' schema exists in the database, and that the logged user has the necessary privileges to create a new table in that schema.

DBI::dbExecute(con, "CREATE TABLE feedback (datetime TIMESTAMP WITH TIME ZONE, user TEXT, bugs TEXT, ui_comment TEXT, module_comment TEXT, got_data BOOLEAN, missing TEXT)")
