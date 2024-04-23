# General notes
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
2. Add comprehensive popup labels to map markers. 
-   Permafrost database app: <https://github.com/YukonGeologicalSurvey/PermafrostDB/blob/master/TempDataApp/app.R> Uses package leafpop, but leafpop seems to be dormant (not updated in 2 years). At least there are few imports and suggests, though with sp as a suggests it's a bit risky.
-   Other options: <https://stackoverflow.com/questions/29173336/how-to-display-advanced-customed-popups-for-leaflet-in-shiny> This seems to be a quite functional example with no additional package dependencies.
## Improvements
- Sort filter options alphabetically
- Create HTML popups for map markers only once, at beginning of session.
- Explore loading data for the app, shared between all sessions, and updating every hour at half past.
