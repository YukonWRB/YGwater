<!-- badges: start -->
[![R-CMD-check](https://github.com/YukonWRB/YGwater/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/YukonWRB/YGwater/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->


#### Welcome to the Yukon Water Resources Branch's YGwater GitHub repository!

This public repository holds many of the R functions that we use on a day-to-day basis at the Water Resources Branch for data visualization, statistical analysis, and reporting purposes. It also holds the R Shiny application that will soon be published (we'll put the URL here once it is).

You'll also find a host of other interesting R functions, some more hydrology/climate specific than others:

-   Functions for checking and installing python dependencies. We use these here for automating the installation of dependencies necessary for testing {plotly} objects, but they've been generalized due to their clear applicability for other tasks;

-   A function to automate the proper capitalization of strings in French or English while respecting Yukon place names;

-   Functions to fetch data from Environment and Climate Change Canada and process it, for example by combining timeseries of different monitoring locations when stations have been relocated over the years;

-   Hydrology-specific functions to check and install the Water Survey of Canada's (WSC) HYDAT database and extract watershed polygons for WSC stations.

-   Plotting functions, which are mostly designed to work directly with our internal database. That said, some have ways of bypassing the database connection by passing a data.frame as an argument.

Other functions are more specific to our work here at the Water Resources Branch, but may still be useful to other governments, academia, or the private sector with similar needs. If this describes you or your organization, please reach out to use and we can discuss how we can help you get set up with the AquaCache database back-end necessary for these functions to work.
