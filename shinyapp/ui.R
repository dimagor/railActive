
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinydashboard)
library(DT)

header <- dashboardHeader(title = "Rail Dashboard",
                          dropdownMenuOutput(outputId = "notifications"),
                          dropdownMenuOutput(outputId = "train_status"))

body <- dashboardBody(
  fluidRow(
    column(width = 4, box(collapsible = FALSE, width = NULL, title = "Control Panel", height = "350px",
                          uiOutput(outputId = "line_list"),
                          br(),
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
    column(width = 3, box(collapsible = FALSE, width = NULL, title = "Live Status", height = "350px",
                          valueBoxOutput("current_delaychance", width = 12),
                          uiOutput(outputId = "currentcond")
                          )
    ),
    column(width = 5, box(collapsible = FALSE, width = NULL, title = "Explanation of Effect", height = "350px",
                          plotOutput(outputId = "featureplot", height = "280px", width = "100%")))
  ),
  fluidRow(
    column(width = 12,
    box(collapsible = FALSE, width = NULL, title = "12-Hour Forecast", height = "450px",
        plotOutput(outputId = "hourlyforecastplot", height = "380px", width = "100%")))
  )
)

dashboardPage(header,  dashboardSidebar(disable = TRUE),  body, skin = "black")
