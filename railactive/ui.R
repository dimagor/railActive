library(shiny)
library(shinydashboard)

header <- dashboardHeader(title = "RailActive",
                          dropdownMenuOutput(outputId = "notifications"),
                          dropdownMenuOutput(outputId = "train_status"))

body <- dashboardBody(
  # Custom style definitions to improve readability
  tags$style(type="text/css", "body{ font-size: 18px;}"),
  tags$style(type="text/css", ".box-header .box-title{ font-size: 24px; }"),
  tags$style(type="text/css", ".main-header .logo{ font-size: 38px; font-weight: 500;}"),

  # Current Predictions + Controls
  fluidRow(
    column(width = 4,
           box(collapsible = FALSE, width = NULL, title = "Control Panel", height = "420px",
               uiOutput(outputId = "line_list"),
               selectInput("interval", label = "Refresh interval",
                           choices = c(
                             "30 seconds" = 30,
                             "1 minute" = 50,
                             "5 minutes" = 600,
                             "Off" = 0),
                           selected = "30"),
               actionButton("refresh", "Refresh now"),
               uiOutput(outputId = "last_update")
           )),
    column(width = 3,
           box(collapsible = FALSE, width = NULL, title = "Live Status", height = "420px",
               valueBoxOutput("current_delaychance", width = 12),
               uiOutput(outputId = "currentcond")
           )
    ),
    column(width = 5,
           box(collapsible = FALSE, width = NULL, title = "Explanation of Effect", height = "420px",
               plotOutput(outputId = "featureplot", height = "350px", width = "100%")))
  ),

  # 12-hour forecast
  fluidRow(
    column(width = 12,
           box(collapsible = TRUE, collapsed = FALSE, width = NULL, title = "12-Hour Forecast",
               plotOutput(outputId = "hourlyforecastplot", height = "380px", width = "100%")))
  )
)

dashboardPage(header,  dashboardSidebar(disable = TRUE),  body, skin = "black")
