library(tidyverse)
library(data.table)
library(zoo)
library(lubridate)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
options(scipen = 999)
setwd("C:/Users/gsgr/Documents/SCPD/STATS290/Project/Stats-290-Project")

## function to get weather a data by location, all other params are optional
getWeatherDataForDateRange  <- function(offline=TRUE,
                                        location,
                                        state,
                                        from=date(),
                                        to=date()
                                        #range=numeric()
                                        ){
  # format inputs
  from <- as.Date(from)
  to <- as.Date(to)
  offline<- offline
  
  # check offline falg
  if(offline==TRUE){
    ## connect to rda file
    load("relevant_offline_database.rda")

    data <- relevant_offline_database%>%
      filter(trim(AREA)==loc,trim(STATE)==st)%>%
      filter(ymd(LST_DATE)>= from & ymd(LST_DATE)<= to)
    
    
    
    ## return data
  }else {
    #   prepare for API
    
    ## load location ID to AREA file
    load("Locations.rda")
    
    ## get id for selected location
    location_ids <- Locations%>%
      filter(state==st,location==loc)%>%
      dplyr::select(id)
      
    ## make API call
    for (i in 1:nrow(location_ids)){
      loc_id<-location_ids$id[i]
      API_URL <- "https://www.ncdc.noaa.gov/crn/api/v1.0/sites/"
      API_URL_final<- paste0(API_URL,loc_id,
                             "/data?start=",from,"T00:00Z&end=",to,
                             "T00:00Z&metric=t_official&metric=t_max&metric=t_min&metric=")
      
      data <- fromJSON(RCurl::getURL(API_URL_final))
      
      if(!is.na(data) & nrow(data)!=0){
        data <- data[,c("start","value","metric")]
        names(data) <- c("time","value","metric")
        data$id <- loc_id
        }
      
      if(exists("weather_data")){
        weather_data <- rbind(weather_data,data)
      } else 
        weather_data <- data
    }
    
  ## return data
    weather_data
  }
  
}