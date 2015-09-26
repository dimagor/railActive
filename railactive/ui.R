
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinydashboard)
library(DT)

header <- dashboardHeader(title = "RailActive",
                          dropdownMenuOutput(outputId = "notifications"),
                          dropdownMenuOutput(outputId = "train_status"))

body <- dashboardBody(
  fluidRow(
    column(width = 12,
           box(collapsible = TRUE, title = "Welcome/About", width = NULL, collapsed = TRUE,
               p(tags$b("Railactive"), "is a dashboard designed to monitor and predict up-to-the minute short term delays for the NJTransit rail system."),
               p("The dashboard panel utilizes a predictive algorithm to forecast the immediate and short-term possibility of a delay occuring for NJT trains. More specifically, the predition reflects how likely it is for a given train line to experience delays of 10 minutes or more at its final destination."),
               h4("Control Panel"),
                p("This section of the control panel allows the user to select the desired line to monitor and to chose how frequently the predictions are refreshed."),
               h4("Live status"),
               p("Provides a forecast of a delay occuring on the selected line as well as relevant information about current travel conditions."),
               h4("Explanation of Effect"),
               p("This plot summarizes how the current conditions are impacting the prediction of train delays for the chosen line."),
               h4("12-Hour Forecast"),
               p("This bottom pannel forecasts the possibility of a delay on the line for the next 12 hours.",
                 tags$b("Color Key:"), "green-yellow suggests a low chance of delay, orange a medium and red a high chance of delay.")
               )
    )
  ),
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
