
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinydashboard)
library(DT)

shinyUI(navbarPage("Delay Predictor",
                   # tabPanel(title = "Home"),
                   tabPanel(title = "Dashboard",
                            fluidRow(style = "background-color: #F8F8F8; border-radius: 5px; border: 2px solid #707070; margin: 2px; padding: 2px; ",
                              column(8, "",
                                     # h2("Current Conditions"),
                                     uiOutput(outputId = "last_update"),
                                     uiOutput(outputId = "currentcond")),
                              column(4, "",
                                selectInput("interval", label = "Refresh interval",
                                            choices = c(
                                              "30 seconds" = 30,
                                              "1 minute" = 50,
                                              "5 minutes" = 600,
                                              "Off" = 0),
                                            selected = "30"),
                                actionButton("refresh", "Refresh now")
                            )),
                            fluidRow(
                              column(6,
                              DT::dataTableOutput(outputId = "predictions")
                              )
                            )
                            ),
                   tabPanel(title = "About")
))
