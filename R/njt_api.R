#' @export
source("apiconfig.R")
library(httr)
library(httpuv)
library(jsonlite)

queryNJTapi <- function(station = "NY"){
base_cmd <- "
curl -X POST -H \"Content-Type: text/xml; charset=utf-8\" -H \"Host: traindata.njtransit.com\" -H \"SOAPAction: http://microsoft.com/webservices/getTrainScheduleJSON\" -H \"Cache-Control: no-cache\" -H \"Postman-Token: 49970434-1a97-b80a-d79c-7fa94bef1dd1\" -d '<?xml version=\"1.0\" encoding=\"utf-8\"?>
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
data.frame(results$STATION$ITEMS$ITEM) %>% select(Scheduled_Start_Time = SCHED_DEP_DATE, Line = LINE, Train = TRAIN_ID, STATUS, SEC_LATE, BACKCOLOR, FORECOLOR, TRAIN_LINE, LINEABBREVIATION)
}
