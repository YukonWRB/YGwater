
dataUI <- function(id) {
  ns <- NS(id)
  tagList(
    # Filters
    selectInput(ns("typeFlt"), "Data Type", choices = c("All")), # choices and labels are updated in the server module
    selectInput(ns("paramFlt"), "Parameter", choices = c("All")),
    selectInput(ns("projFlt"), "Project", choices = c("All")),
    selectInput(ns("netFlt"), "Network", choices = c("All")),
    actionButton(ns("reset"), "Reset Filters"),
    actionButton(ns("view"), "View Data"),
  )
}


data <- function(id, con, language, restoring) {
  moduleServer(id, function(input, output, session) {
    
    setBookmarkExclude(c("reset", "view"))
    data_types <- DBI::dbGetQuery(con, "SELECT p.* FROM param_types AS p WHERE EXISTS (SELECT 1 FROM timeseries t WHERE t.param_type = p.param_type_code);")
    parameters <- DBI::dbGetQuery(con, "SELECT p.* FROM parameters AS p WHERE EXISTS (SELECT 1 FROM timeseries t WHERE t.parameter = p.param_code);")
    parameters <- DBI::dbGetQuery(con, "SELECT * FROM parameters")
    projects <- DBI::dbGetQuery(con, "SELECT * FROM projects")
    networks <- DBI::dbGetQuery(con, "SELECT * FROM networks")
    
    observe({
      lang <- language()
      abbrev <- translations[translations$id == "titleCase", ..lang][[1]]
      
      updateSelectInput(session, 
                        "typeFlt",
                        label = translations[translations$id == "data_type", ..lang][[1]],
                        choices = c(translations[translations$id == "all", ..lang][[1]],
                                    titleCase(data_types$param_type, abbrev)))
      
      updateSelectInput(session,
                        "paramFlt",
                        label = translations[translations$id == "parameter", ..lang][[1]],
                        choices = c(translations[translations$id == "all", ..lang][[1]], 
                                    titleCase(parameters[[translations[translations$id == "param_name_col", ..lang][[1]]]], 
                                              abbrev)))
      
      updateSelectInput(session,
                        "projFlt",
                        label = translations[translations$id == "project", ..lang][[1]],
                        choices = c(translations[translations$id == "all", ..lang][[1]],
                                    titleCase(projects[[translations[translations$id == "generic_name_col", ..lang][[1]]]], 
                                              abbrev)))
      
      updateSelectInput(session,
                        "netFlt",
                        label = translations[translations$id == "network", ..lang][[1]],
                        choices = c(translations[translations$id == "all", ..lang][[1]],
                                    titleCase(networks[[translations[translations$id == "generic_name_col", ..lang][[1]]]], 
                                              abbrev)))
    })
  })
}
