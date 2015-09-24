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
  getTTLD <- reactive({
    read.csv("http://52.88.4.39/ttld_log.txt")
  })

  buildCurrentPredictionDF <- reactive({
    input$refresh
    interval <- max(as.numeric(input$interval), 5)
    if(input$interval != 0) invalidateLater(interval * 1000, session)

    #A parameter of the model (time to delay is constrained to a max of 24 hours)
    max_delay = 24*60

    current_weather <- getCurrentWeather()
    ttld_file <- getTTLD()

    # Process Date/Time
    pred_df <- main_info %>% mutate(Current_Datetime = Sys.time(), #make sure system time is correct on server
                                    dep_hour = hour(Current_Datetime),
                                    dep_mon = month(Current_Datetime, label = TRUE),
                                    dep_wday = wday(Current_Datetime, label = TRUE))

    # Retriev & Process previous delays that were more than 10 minutes long

    pred_df <- ttld_file %>%
      select(ttld_Line = Line, Scheduled_Start_Time) %>%
      right_join(pred_df) %>%
      mutate(ttl_line = as.numeric(difftime(Current_Datetime, Scheduled_Start_Time, units = "mins"))) %>%
      mutate(ttl_line = ifelse(is.na(ttl_line)|ttl_line > max_delay, max_delay, ttl_line))

    # Obtain current weather for Newark

    pred_df$Temp_F <- current_weather["Temp_F"]
    pred_df$Visibility <- current_weather["Visibility"]
    pred_df$WindSpeed <- current_weather["WindSpeed"]
    return(pred_df)
  })

  buildHourlyWeatherDF <- reactive({
    input$refresh
    interval <- max(as.numeric(input$interval), 5)
    if(input$interval != 0) invalidateLater(interval * 1000, session)

    current_weather <- getCurrentWeather()
    ttld_file <- getTTLD()

    w_get_hourly <- GET("http://api.wunderground.com/api/547710b840da62b4/hourly/q/NJ/Newark.json")
    w_text_hourly <- content(w_get_hourly, as = "text") %>% fromJSON()

    w_text_hourly$hourly_forecast %>% flatten %>%
      transmute(WindSpeed = as.numeric(wspd.english),
                Temp_F = as.numeric(temp.english),
                Date_Time = paste(FCTTIME.hour_padded,
                                  FCTTIME.min,
                                  FCTTIME.mon_padded,
                                  FCTTIME.mday_padded,
                                  FCTTIME.year)) %>%
      mutate(Date_Time = as.POSIXct(strptime(Date_Time, format = "%H %M %m %d %Y")),
             Visibility = current_weather["Visibility"],
             dep_hour = hour(Date_Time),
             dep_mon = month(Date_Time, label = TRUE),
             dep_wday = wday(Date_Time, label = TRUE)) %>%
      slice(1:12)
  })

  buildHourlyPredictionDF <- reactive({
    hourly_weather <- buildHourlyWeatherDF()
    pred_df <- hourly_weather %>% mutate(Line = input$trainline) %>% inner_join(main_info, by = "Line")
    ttld_file %>%
      select(ttld_Line = Line, Scheduled_Start_Time) %>%
      right_join(pred_df, by = "ttld_Line") %>%
      mutate(ttl_line = as.numeric(difftime(Date_Time, Scheduled_Start_Time, units = "mins"))) %>%
      mutate(ttl_line = ifelse(is.na(ttl_line)|ttl_line > max_delay, max_delay, ttl_line))
  })

  predictCurrentProbabilities <- reactive({
    pred_df <- buildCurrentPredictionDF()
    if(!is.null(pred_df)){
    inner_join(pred_df, modelfit_byline, by = "Line")  %>%
      mutate(Line_Name = Full.Name) %>%
      group_by(Line_Name, Line, dep_hour)  %>%
      do(data.frame(p=predict(.$fit, ., type = "prob")[[1]][[1]][1])) %>%
      ungroup %>%
        mutate(pct = as.integer(round(p*100)),
             txt = ifelse(pct<50, "low",ifelse(pct<75,"medium", "high")))
    }else{NULL}
  })

  predictHourlyProbabilities <- reactive({
    pred_df <- buildHourlyPredictionDF()
    if(!is.null(pred_df)){
      inner_join(pred_df, modelfit_byline, by = "Line")  %>%
        mutate(Line_Name = Full.Name) %>%
        group_by(Line_Name, Line, dep_hour,  Date_Time)  %>%
        do(data.frame(p=predict(.$fit, ., type = "prob")[[1]][[1]][1])) %>%
        ungroup %>%
        mutate(pct = as.integer(round(p*100)))
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

  output$notifications <- renderMenu({
    current_prob <- predictCurrentProbabilities()
    if(!is.null(current_prob)){
      current_prob <- current_prob %>% ungroup %>% arrange(-pct)
      messages <- current_prob %>% count(txt)  %>%
        mutate(icontype = ifelse(txt == "low",
                                 "check" ,
                                 ifelse(txt == "medium",
                                        "info-circle" ,
                                        "exclamation-triangle"))) %>%
        mutate(message_txt = paste(n,"trains have a", txt, "chance of delay"))
      max_chance <- max(current_prob$pct)
      badge_status <- if(max_chance < 50) {"success"
      }else if(max_chance <75) {"warning"
      }else {"danger"}
      progress_msgs <- apply(messages, 1, function(row) {
        notificationItem(text = row[["message_txt"]],
                         icon = icon(row[["icontype"]])
        )
      })
    }
    else progress_msgs = list()
    dropdownMenu(type = "notification", .list = progress_msgs, badgeStatus = badge_status)
  })

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
    selectInput(inputId = "trainline", label = "Train Line:", line_list, selected = 1)
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
    if(!is.null(current_prob) & !is.null(input$trainline)){
      train <- current_prob[current_prob$Line == input$trainline, ]
      color = getStatusColor(train$pct)
      txt = toupper(train$txt)
      valueBox(value = txt, subtitle = "chance of delay", color = color, width = 12)
    }else {valueBox(value = "Loading", subtitle = "", width = 12)}
  })

  output$currentcond <- renderUI({
    current_prob <- predictCurrentProbabilities()
    current_cpt <- buildCurrentCPT()
    #Need to functionalize this call
    if(!is.null(current_prob) & !is.null(current_cpt) & !is.null(input$trainline)){
      cpt_line <- current_cpt[current_cpt$Line == input$trainline, ]
      vals <- cpt_line %>% select(-p_fold) %>% spread(feature, feature_val)
      ttl_line <- as.numeric(vals$ttl_line)
      last_delay <- if(ttl_line<120){
        paste(round(ttl_line),"mins")
      }else if(ttl_line < 1440){
        paste(round(ttl_line/60,digits = 2), "hours")
      }else {">24 hours"}
      tags$table(style = "width = 100%",
                 tags$tr(tags$td(tags$b("Last Delay:"), last_delay)),
                 tags$tr(tags$td(tags$b("Temp:"), paste0(vals$Temp_F,"°F"))),
                 tags$tr(tags$td(tags$b("WindSpeed:"), paste0(vals$WindSpeed,"mph"))),
                 tags$tr(tags$td(tags$b("Visibility:"), paste0(vals$Visibility,"miles")))

      )
    }
    else {NULL}
  })
  output$featureplot <- renderPlot({
    current_cpt <- buildCurrentCPT()
    if(!is.null(current_prob) & !is.null(current_cpt) & !is.null(input$trainline)){
      cpt_line <- current_cpt[current_cpt$Line == input$trainline, ]
      cpt_line <- cpt_line %>% mutate(feature = factor(feature,
                                                              levels = rev(c("ttl_line_break",
                                                                         "Temp_F_break",
                                                                         "WindSpeed_break",
                                                                         "Visibility_break",
                                                                         "dep_hour",
                                                                         "dep_wday",
                                                                         "dep_mon")),
                                                              labels = rev(c("Time To\nLast Delay",
                                                                         "Current\nTemperature",
                                                                         "Current\nWind Speed",
                                                                         "Current\nVisibility",
                                                                         "Time of Day",
                                                                         "Day of\nthe Week",
                                                                         "Current\nMonth"))
      ))
      cpt_line %>% ggplot(aes(feature,p_fold, fill = p_fold)) +
        geom_bar(stat = "identity") +
        scale_fill_gradient2(low = "blue", mid = "black", high = "red", midpoint = 0, limits=c(-2, 2), guide = FALSE) +
        scale_y_continuous(limits = c(-2,2),
                           breaks = c(-2,0,2),
                           labels = c("Decreases\nDelay\nChance","Neutral", "Increases\nDelay\nChance")) +
        xlab("") + ylab("") +
        coord_flip() +
        theme_minimal() +
        theme(axis.text=element_text(size=12))
    }
    else{NULL}
  })
  output$hourlyforecastplot <- renderPlot({
    hourly_prob <- predictHourlyProbabilities()
    if(!is.null(hourly_prob) & !is.null(input$trainline)){
      hourly_prob  %>%
        ggplot(aes(Date_Time, pct, fill = pct)) +
        geom_bar(stat = "identity") +
        scale_y_continuous(limits = c(0,100),
                           breaks = c(25, 67, 87),
                           labels = c("Low", "Medium", "High")) +
        scale_fill_gradient2(low = "chartreuse4", mid = "yellow", high = "red4", midpoint = 75, limits=c(0, 100), guide = FALSE) +
        xlab("") + ylab("") +
        theme_minimal() +
        theme(axis.text=element_text(size=12))
    }
    else {NULL}
  })
})
