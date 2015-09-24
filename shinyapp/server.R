# TODO: Merge the repos into a single R repository
# TODO: Build fault tolerance into app (i.e. maximum wait time & caching for demo)
library(shiny)
library(shinydashboard)
library(lubridate)
library(httr)
library(jsonlite)
library(DT)
library(dplyr)
library(tidyr)

shinyServer(function(input, output, session) {
  main_info <- read.csv("main_info.csv")
  load(file = "modelfit_byline.rda")
  load(file = "cpt_complete.rda")

  getCurrentWeather <- reactive({
    hourly_newark <- "http://forecast.weather.gov/MapClick.php?lat=40.7242&lon=-74.1726&FcstType=json"
    w_get <- GET(hourly_newark)
    w_text <- content(w_get, as = "text") %>% fromJSON()
    c("Temp_F" = as.numeric(w_text$currentobservation$Temp[1]),
    "Visibility" = as.double(w_text$currentobservation$Visibility[1]),
    "WindSpeed" = as.numeric(w_text$currentobservation$Winds[1]))
  })

  buildCurrentPredictionDF <- reactive({
    input$refresh
    interval <- max(as.numeric(input$interval), 5)
    if(input$interval != 0) invalidateLater(interval * 1000, session)

    #A parameter of the model (time to delay is constrained to a max of 24 hours)
    max_delay = 24*60

    current_weather <- getCurrentWeather()

    # Process Date/Time
    pred_df <- main_info %>% mutate(Current_Date = Sys.time(), #make sure system time is correct on server
                                    dep_hour = hour(Current_Date),
                                    dep_mon = month(Current_Date, label = TRUE),
                                    dep_wday = wday(Current_Date, label = TRUE))

    # Retriev & Process previous delays that were more than 10 minutes long
    ttld_file <- read.csv("http://52.88.4.39/ttld_log.txt")
    pred_df <- ttld_file %>%
      select(ttld_Line = Line, Scheduled_Start_Time) %>%
      right_join(pred_df) %>%
      mutate(ttl_line = as.numeric(difftime(Current_Date, Scheduled_Start_Time, units = "mins"))) %>%
      mutate(ttl_line = ifelse(is.na(ttl_line)|ttl_line > max_delay, max_delay, ttl_line))

    # Obtain current weather for Newark

    pred_df$Temp_F <- current_weather["Temp_F"]
    pred_df$Visibility <- current_weather["Visibility"]
    pred_df$WindSpeed <- current_weather["WindSpeed"]
    return(pred_df)
  })

  predictCurrentProbabilities <- reactive({
    pred_df <- buildCurrentPredictionDF()
    if(!is.null(pred_df)){
    inner_join(pred_df, modelfit_byline, by = "Line")  %>%
      mutate(Line_Name = Full.Name) %>%
      group_by(Line_Name, Line)  %>%
      do(data.frame(p=predict(.$fit, ., type = "prob")[[1]][[1]][1])) %>%
      ungroup %>%
        mutate(pct = as.integer(round(p*100)),
             txt = ifelse(pct<50, "low",ifelse(pct<75,"medium", "high")))
    }else{NULL}
  })
  buildCurrentCPT <- reactive({
    pred_df <- buildCurrentPredictionDF()
    if(!is.null(pred_df)){
      pred_df %>% mutate(
        ttl_line_break = cut(ttl_line, breaks_ttl_line , include.lowest = TRUE),
        Temp_F_break = cut(Temp_F, breaks_Temp_F , include.lowest = TRUE),
        WindSpeed_break = cut(WindSpeed, breaks_WindSpeed , include.lowest = TRUE),
        Visibility_break = cut(Visibility, breaks_Visibility , include.lowest = TRUE)
      ) %>%
        select(Line, dep_hour:Visibility_break) %>% gather(feature, feature_val, -Line, -ttl_line:-WindSpeed) %>%
        inner_join(cpt_complete) %>%
        mutate(impact = ifelse(p_fold < -1, "Lowers Chance", ifelse(p_fold > 1, "Increaes Chance", "Neutral")))
    }else{NULL}
  })

  getStatusColor <- function(value){
    if(value < 50){
      "green"
    }else if(value < 75){
      "yellow"
    } else {"red"}
  }

  # Header ---

  output$train_status <- renderMenu({
    current_prob <- predictCurrentProbabilities()
    if(!is.null(current_prob)){
      current_prob <- current_prob %>% ungroup %>% arrange(-pct)
      max_chance <- max(current_prob$pct)
      badge_status <- if(max_chance < 50) {"success"
      }else if(max_chance <75) {"warning"
      }else {"danger"}
      progress_msgs <- apply(current_prob, 1, function(row) {
        taskItem(value = row[["pct"]],
                 color = getStatusColor(row[["pct"]]),
                 text = paste(row[["Line_Name"]])
        )
      })
    }
    else progress_msgs = list()
    dropdownMenu(type = "tasks", .list = progress_msgs, badgeStatus = badge_status)
  })

  # Body ---------
  output$line_list <- renderUI({
    line_list <- as.list(main_info$Line)
    names(line_list) <- main_info$Full.Name
    selectInput(inputId = "trainline", label = "Train Line:", line_list)
  })
#
#   output$predictions <- renderDataTable({
#     predictCurrentProbabilities() %>%
#       datatable(options = list(dom = 't'), rownames = FALSE) %>%
#       formatStyle('p', backgroundColor = styleInterval(c(0.5,0.75), c('green','yellow','red')))
#     })

  output$last_update <- renderUI({
    input$refresh
    interval <- max(as.numeric(input$interval), 5)
    if(input$interval != 0) invalidateLater(interval * 1000, session)
    p(class = "muted",
      tags$b("Predictions as of:"),
      br(),
      format(Sys.time(), "%A %B %e, %I:%M%p")
      )
  })

  output$current_delaychance <- renderValueBox({
    current_prob <- predictCurrentProbabilities()
    if(!is.null(current_prob)){
      pct <- current_prob[current_prob$Line == input$trainline, "pct"][1]
      valueBox(value = paste(pct, "%"), subtitle = "chance of delay", color = getStatusColor(pct))
    }else {NULL}
  })

#   output$currentcond <- renderUI({
#     current_prob <- predictCurrentProbabilities()
#     current_cpt <- buildCurrentCPT()
#     if(!is.null(current_prob) & !is.null(current_cpt)){
#       txt <- current_prob[current_prob$Line == as.character(input$trainline), "txt"][[1]]
#       cpt_line <- current_cpt[current_cpt$Line == input$trainline, ]
#       cpt <- cpt_line  %>% select(-feature_val,-p_fold) %>% spread(feature, impact)
#       vals <- cpt_line %>% select(-p_fold, -impact) %>% spread(feature, feature_val)
#       ttl_line <- as.numeric(vals$ttl_line)
#       ttl_str <- if(ttl_line < 120){
#         paste(ttl_line,"minutes ago")
#       }else if(ttl_line < 1440){
#         paste(round(ttl_line/60),"hours ago")
#       }else {paste("More than 24h ago")}
#       temp_str <- paste0(vals$Temp_F,"°F")
#       speed_str <- paste0(vals$WindSpeed,"mph")
#       vis_str <- as.character(vals$Visibility)
#       tags$div(
#         p(paste0("The chance of delay is ",txt,"."), style="font-size: 20px;"),
#       tags$table(style="width:100%",
#                  tags$tr(
#                    tags$th("Temperature:"),
#                    tags$th("WindSpeed:"),
#                    tags$th("Visibility:"),
#                    tags$th("Last Delay:")
#                  ),
#                  tags$tr(
#                    tags$td(temp_str),
#                    tags$td(speed_str),
#                    tags$td(vis_str),
#                    tags$td(ttl_str)
#                  )
# #                  tags$tr(
# #                    tags$td(cpt$Temp_F_break),
# #                    tags$td(cpt$WindSpeed_break),
# #                    tags$td(cpt$Visibility_break),
# #                    tags$td(cpt$ttl_line_break)
# #                  )
# )
#       )
#     }else NULL
#   })

})
