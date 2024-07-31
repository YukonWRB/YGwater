#globals for aq_download
utils::globalVariables(c("timeseries"))

#globals for hydrometContinuous
utils::globalVariables(c("datetime_UTC", "value"))

#globals for Shiny applications that source a script
utils::globalVariables(c("hydroApp_globals", "streamLine_globals"))
