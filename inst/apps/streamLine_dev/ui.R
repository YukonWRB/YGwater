
ui <- dashboardPage(
  header = dashboardHeader(
    title = "StreamLine",
    rightUi = tagList(
      rightUi = dropdownMenu(
        type = "notifications",
        badgeStatus = "primary",
        taskItem(
          textInput("languageToggle", label = NULL, value = "Français")
        )
      )
    )
  ),
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "HomeView", icon = icon("home")),
      menuItem("Map View", tabName = "MapView", icon = icon("map"))
      # Add other menu items here
    )
  ),
  body = dashboardBody(
    useShinyjs(),
    tabItems(
      # Home View Tab
      tabItem(tabName = "HomeView",
              h2(textOutput("home_title")),
              fluidPage(uiOutput("homeContent"))
      ),
      # Map View Tab
      tabItem(tabName = "MapView",
              h2(textOutput("map_title")),
              mapViewUI("map_view", translations = translations)
      )
      # Add other tabs here
    )
  ),
  title = "StreamLine"
)
