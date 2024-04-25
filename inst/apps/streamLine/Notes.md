# General notes
## Notes
Sending values to the main server from modules:
1. Assign a variable name to the module server function, e.g. `server <- function(input, output, session) { ... }`
2. return() something from the server. AFAIK, this needs to be a reactiveVal or reactiveValues object.
3. observe() the variable name you assigned to the module.


## Issues
- Reduce or eliminate all browser "inspect" errors and warnings
## Improvements
- Add a "loading" message when the app is loading data
- Add a partner login option, which will open up public = FALSE timeseries and possibly tabs like the images tab.
## TODOs
- Make sure URL only gets necessary input parameters
- Make sure restoration from URL works
- Explore forward and back buttons in browser

# Image view
## Issues
- Keyboard arrow use conflicts with DT running on server. Troubleshoot and determine if this should be kept.

## Improvements
- Modal map for location selection

# Documents view
## Issues

## Improvements
- Modal map for location selection

# Data view
## Improvements
- Export as .csv: use datatable extensions instead of separate workflow. See item 2 here: <https://rstudio.github.io/DT/extensions.html>


# Map view
## Issues
1. Map filters do not cross-update, i.e. selecting a filter that makes other filter options irrelevant does not remove those options from the other filters.
## Improvements
- Sort filter options alphabetically
