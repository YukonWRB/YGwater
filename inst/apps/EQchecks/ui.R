# UI definition for EQchecks application.

# EQchecks consists of a simple sidebar and main panel layout.

# The sidebar contains menus and buttons to allow the user to select a date range in which to check samples, optional menus allowing them to limit checks to location(s) or even specific samples based on datetime. A 'Run checks' button triggers the checks, the result of which will show up in the main panel
# Optionally, we can add a collapsible set of buttons to enable/disable specific checks.

# The main panel contains a data.table with a row for each sample which failed at least one check, with metadata for location, datetime, failed checks (as string). The user can then click on individual rows which brings up a modal (pop-up) with basic medatata at the top and a table with details of the checks (failed and not) below. There is also a button to export the check result below the table.

