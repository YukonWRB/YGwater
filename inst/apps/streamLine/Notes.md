# General notes and todos
- Normalize database further.
  - timeseries.period_type, timeseries.category (maybe? It's just two options)
- Add audit document type
- Implement use of hourly data views table for plots
- continuous/discrete/FORECAST?? or forecast points its own drop-down??? (it would not appear until at least one forecast location is created)
  - Forecasts table needs additional model information, possibly new cols for 95% CI, min/max of ensemble, ensemble or not, number of members, ....
- Create workflow for importing HDRPS/RDPS direct to DB
  - If implementing a ML model for flow prediction, import other parameters: temp, solrad, wind??, 
- Add HRDPA stored in files to the DB

## Feedback from Sean Boots
- Change title to something like "Yukon Water Data", only visible on the tab or browser title. No need for a title on the page itself as it should be obvious to the user.
- YG logo replaces aurora, aurora under login/lang side
- Remove login option entirely, make a separate URL for partners (that has a login on page initial access)
- If only doing two languages, make language drop-down a click to the other language
- Remove email link, put in About tab
- Add link to general YG disclaimer in footer, plus other footer items like privacy policy, etc. as in the Flood Atlas that links to the YG website
- Add notes about data licences, i.e from ECCC, USGS, etc. (Talk to Sean about what these licences actually are)


## Issues
- Reduce or eliminate all browser "inspect" errors and warnings
## Improvements
- Add a "loading" message when the app is loading data
- Allow user to extend the time range of plotly plots by panning.
## TODOs
- Make sure URL only gets necessary input parameters
- Make sure restoration from URL works
- Explore forward and back buttons in browser


# Image view
## Issues
- Keyboard arrow use conflicts with DT running on server. Troubleshoot and determine if this should be kept.
## Improvements
- Implement login if statements
- Modal map for location selection


# Documents view
## Issues
## Improvements
- Implement login if statements
- Modal map for location selection


# Data view
## TODO
## Improvements
- Implement login if statements
## Issues
- Hover buttons conflict with language selection drop-down. Troubleshoot and determine if this should be kept. (Is it a problem with running on server?)
- Navigating away from data tab when a modal is open causes a crash. Automatically close the modal when navigating away.

# Map view
## Issues
- Map filters do not cross-update, i.e. selecting a filter that makes other filter options irrelevant does not remove those options from the other filters.
- Implement login if statements
- Locations do not show up if they don't have a timeseries. This is a problem for images and documents. Add checkbox for "Show locations with only images" or "Show locations with only documents".
## Improvements
- Sort filter options alphabetically


# Plot view
## TODO
- Allow user to made a second plotting area for comparison.
## Improvements
- Implement login if statements
## Issues

