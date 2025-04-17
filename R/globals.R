#globals for aq_download
utils::globalVariables(c("timeseries"))

#globals for ggplotOverlap
utils::globalVariables(c("datetime_UTC", "value"))

#globals for Shiny applications that source a script
utils::globalVariables(c("floodAtlas_ts_globals", "floodAtlas_over_globals", "YGwater_globals"))



#globals related to use of data.table
utils::globalVariables(c("."))
