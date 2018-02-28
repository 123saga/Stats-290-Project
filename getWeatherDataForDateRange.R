library(dplyr)
library(tidyr)
library(data.table)
library(zoo)
library(lubridate)


## function to get weather a data by location/date range
getWeatherDataForDateRange  <- function(AREA=NA,STATE=NA,from,to){
  setwd("C:/Users/gsgr/Documents/SCPD/STATS290/Project/Stats-290-Project")
  load('database.rda')
  allfiles$LST_DATE<- ymd(allfiles$LST_DATE)
  from<- as.Date(from)
  to<- as.Date(to)
  if(is.na(AREA) && is.na(STATE)){
    return_data <- subset(allfiles,LST_DATE>=from & LST_DATE<=to)
    } else{
        return_data <- subset(allfiles,
                          LST_DATE>=from &
                            LST_DATE<=to &
                            STATE==STATE & 
                            AREA==AREA)
        }
  
    return_data
  
}