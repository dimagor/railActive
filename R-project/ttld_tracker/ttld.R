#!/usr/bin/Rscript

source("apiconfig.R")
library(httr)
library(httpuv)
library(jsonlite)
# library(plyr)
library(dplyr)

ttld_file = "ttld_log.txt"
delay = 60

queryNJTapi <- function(station){
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
  ret_xml <- system(cmd, intern=TRUE)
  ## line breaks removed
  ret_xml <- paste0(ret_xml, collapse="")

  ## 3 The json i
  ret_json <- {  XML::xmlToList(ret_xml) }$Body$getTrainScheduleJSONResponse$getTrainScheduleJSONResult

  ## 4 PARSE THE JSON
  results <- jsonlite::fromJSON(ret_json)
  results_item <- results$STATION$ITEMS$ITEM
  if(!is.null(results_item)){
  data.frame(results_item) %>% select(Scheduled_Start_Time = SCHED_DEP_DATE, Line = LINE, STATUS, SEC_LATE, TRAIN_LINE, LINEABBREVIATION)}
  else(data.frame(NULL))
}

#Using the top 4 Arrival terminals (by number of trains) from each line in 2015
terminals <- data.frame(Terminal = c("NY", "AC", "PH", "HB", "SF", "HS", "LB", "TR", "DO", "GL", "MV", "NW", "RA", "SU", "WC"))
# terminals <- c("NY", "AC", "PH", "LW", "HB", "SF", "WC", "HS", "LK", "LB", "BH", "TR", "JA", "DO", "GL", "SU", "MV", "SV", "NW", "RA", "HI", "HH", "RW")

# If initializing for the first time load in the current file
current_ttld <- read.csv(ttld_file)

while(TRUE){
  # Query the NJT API
  print("Updating...")
  results <- terminals %>% group_by(Terminal) %>% do(queryNJTapi(.$Terminal))
  #Add the last delay events
  # results <- rbind(current_ttld)
  current_ttld <- results  %>% group_by(Line)  %>% filter(SEC_LATE >= 600)  %>% arrange(desc(Scheduled_Start_Time))  %>% slice(1)
  #Save file
  write.csv(current_ttld, file = ttld_file, row.names = FALSE)
  Sys.sleep(delay)
}
