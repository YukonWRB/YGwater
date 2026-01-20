adminLandingUI <- function(id) {
  ns <- NS(id)
  page_fluid(
    tags$div(
      tags$h2("Admin overview"),
      tags$p(
        "Welcome to the admin side of YGwater. Use the tabs in the Admin menu to manage data-entry workflows, users, and system content. Some tools are restricted based on your permissions. If you do not see a tab described below, you do not currently have access."
      ),
      tags$h3("What you can do here"),
      tags$ul(
        tags$li(
          tags$strong("Change your password"),
          " — update your login credentials."
        ),
        tags$li(
          tags$strong("Add and manage locations"),
          " — create new locations, modify their metadata including ownership and network associations."
        ),
        tags$li(
          tags$strong("Add and manage timeseries"),
          " — create new timeseries, modify their metadata including automatic import workflows, or manually add data."
        ),
        tags$li(
          tags$strong("Add and manage samples"),
          " — add new samples or modify existing ones"
        ),
        tags$li(
          tags$strong("Add and manage images or documents"),
          " — add new documents or images, linking them spatially to existing locations if needed."
        ),
        tags$li(
          tags$strong("Manage users"),
          " — create user accounts and assign roles (if authorized)."
        ),
        tags$li(
          tags$strong("Manage notifications"),
          " — configure system alerts and automated messages."
        ),
        tags$li(
          tags$strong("Update news page content"),
          " — edit text and images that appear on the public news page."
        ),
        tags$li(
          tags$strong("View feedback"),
          " — review user-submitted feedback."
        )
      ),
      tags$p(
        "Need additional access? Contact an administrator to update your role."
      )
    )
  )
}

adminLanding <- function(id) {
  moduleServer(id, function(input, output, session) {})
}
