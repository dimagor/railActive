# TODO: Merge the repos into a single R repository
# TODO: Build fault tolerance into app (i.e. maximum wait time & caching for demo)
library(shiny)
library(lubridate)
library(httr)
library(jsonlite)
library(DT)

shinyServer(function(input, output, session) {
  main_info <- read.csv("main_info.csv")
  load(file = "modelfit_byline.rda")

  getCurrentWeather <- reactive({
    hourly_newark <- "http://forecast.weather.gov/MapClick.php?lat=40.7242&lon=-74.1726&FcstType=json"
    w_get <- GET(hourly_newark)
    w_text <- content(w_get, as = "text") %>% fromJSON()
    c("Temp_F" = as.numeric(w_text$currentobservation$Temp),
    "Visibility" = as.double(w_text$currentobservation$Visibility),
    "WindSpeed" = as.numeric(w_text$currentobservation$Winds))
  })
  buildCurrentPredictionDF <- reactive({
    input$refresh
    interval <- max(as.numeric(input$interval), 5)
    print(input$interval)
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
    inner_join(pred_df, modelfit_byline, by = "Line")  %>%
      mutate(Line_Name = Full.Name) %>%
      group_by(Line_Name)  %>%
      do(data.frame(p=predict(.$fit, ., type = "prob")[[1]][[1]][1]))
  })


  # Body ---------
  output$predictions <- renderDataTable({
    predictCurrentProbabilities() %>%
      datatable(options = list(dom = 't'), rownames = FALSE) %>%
      formatStyle('p', backgroundColor = styleInterval(c(0.5,0.75), c('green','yellow','red')))
    })

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
  output$currentcond <- renderUI({
    current_weather <- getCurrentWeather()
    tags$table(style="width:100%",
      tags$tr(
        tags$th("Temperature:"),
        tags$th("WindSpeed:"),
        tags$th("Visibility:")
      ),
      tags$tr(
        tags$td(paste0(current_weather["Temp_F"],"°F")),
        tags$td(paste0(current_weather["WindSpeed"],"mph")),
        tags$td(current_weather["Visibility"])
      )
    )
  })

})
