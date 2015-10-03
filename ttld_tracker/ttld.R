#!/usr/bin/Rscript
# Description:
#     This is a small Rscript that collects API data on a regular interval to determine and record the last time a train was delayed
#     for more than 10 minutes. This is required for the classifier to predict train delays.
source("apiconfig.R")
library(httr)
library(httpuv)
library(jsonlite)
library(dplyr)

ttld_file = "ttld_log.txt"
delay = 75

queryNJTapi <- function(station){
  current_time = Sys.time() #Make sure TZ is correst (EDT)
  base_cmd <- "
  curl --silent -X POST -H \"Content-Type: text/xml; charset=utf-8\" -H \"Host: traindata.njtransit.com\" -H \"SOAPAction: http://microsoft.com/webservices/getTrainScheduleJSON\" -H \"Cache-Control: no-cache\" -H \"Postman-Token: 49970434-1a97-b80a-d79c-7fa94bef1dd1\" -d '<?xml version=\"1.0\" encoding=\"utf-8\"?>
  <soap:Envelope xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">
  <soap:Header>
  <UserCredentials xmlns=\"http://microsoft.com/webservices/\">
  <userName>%s</userName>
  <password>%s</password>
  </UserCredentials>
  </soap:Header>
  <soap:Body>
  <getTrainScheduleJSON xmlns=\"http://microsoft.com/webservices/\">
      <station>%s</station>
    </getTrainScheduleJSON>
  </soap:Body>
</soap:Envelope>' 'http://traindata.njtransit.com:8090/NJTTrainData.asmx?wsdl'
"
  cmd <- sprintf(base_cmd, njt_username, njt_password, station)

  # Catch errors/warnings and return a blank data frame
  ret_xml <- tryCatch(system(cmd, intern=TRUE),
                      error = function(c) NULL,
                      warning = function(c) NULL)


  if(is.null(ret_xml)){
    warning(paste("An issue occurred with querying NJTransit API with station",station))
    return(data.frame(NULL))
  }

  # Remove linebreaks
  ret_xml <- paste0(ret_xml, collapse="")

  ret_json <- { XML::xmlToList(ret_xml) }$Body$getTrainScheduleJSONResponse$getTrainScheduleJSONResult

  results <- jsonlite::fromJSON(ret_json)
  results_item <- results$STATION$ITEMS$ITEM
  if(!is.null(results_item)){
    data.frame(results_item) %>%
      select(Scheduled_Start_Time = SCHED_DEP_DATE,
             Line = LINE,
             STATUS,
             TRAIN_LINE,
             LINEABBREVIATION) %>%
      mutate(Scheduled_Start_Time = as.POSIXct(strptime(Scheduled_Start_Time, format="%T %m/%d/%Y")),
             SEC_LATE = difftime(current_time, Scheduled_Start_Time, units = "secs"))
  }
  else(data.frame(NULL))
}

#Using the top 4 Arrival terminals (by number of trains) from each line in 2015
terminals <- data.frame(Terminal = c("NY", "AC", "PH", "HB", "SF", "HS", "LB", "TR", "DO", "GL", "MV", "NW", "RA", "SU", "WC"))

# If initializing for the first time load in the current file
if(file.exists(ttld_file)){
  current_ttld <- read.csv(ttld_file)
} else{current_ttld <- data.frame()}

while(TRUE){
  # Query the NJT API
  print("Updating...")
  results <- terminals %>%
    group_by(Terminal) %>%
    do(queryNJTapi(.$Terminal))

  # Execute if previous function succeeded
  if(length(results)>1){
    #Add the last delay events
    results <- rbind(results,current_ttld)

    #Get the top delays that are more than 10 Minute
    current_ttld <- results  %>%
      group_by(Line) %>%
      filter(SEC_LATE >= 600 | STATUS == "Cancelled") %>%
      arrange(desc(Scheduled_Start_Time)) %>%
      slice(1)
    #Save file
    write.csv(current_ttld, file = ttld_file, row.names = FALSE)
  }
  Sys.sleep(delay)
}
