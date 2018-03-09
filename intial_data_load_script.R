library(tidyverse)
library(data.table)
library(zoo)
library(lubridate)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
options(scipen = 999)
setwd("C:/Users/gsgr/Documents/SCPD/STATS290/Project/Stats-290-Project")

## params chage to jan-jun
from <- as.Date("2017-07-01")
to <- as.Date("2017-12-31")

# downlaod from git
#load("Locations.rda")

 location_ids <- Locations%>%
   dplyr::select(id)
 
 ## progress 
 pb <- txtProgressBar(min = 0, max = nrow(location_ids), style = 3)
 
 
## make API call
for (i in 1:nrow(location_ids)){
  loc_id<-location_ids$id[i]
  API_URL <- "https://www.ncdc.noaa.gov/crn/api/v1.0/sites/"
  API_URL_final<- paste0(API_URL,loc_id,
                         "/data?start=",from,"T00:00Z&end=",to,
                         "T00:00Z&metric=t_official&metric=t_max&metric=t_min&metric=ws_max",
                         "&metric=windspd&metric=rh_std&metric=solarad&metric=p_official")
  
  
  data <- fromJSON(RCurl::getURL(API_URL_final))
  
  if(is.data.frame(data)){
    data <- data[,c("start","value","metric")]
    names(data) <- c("time","value","metric")
    data$id <- loc_id
    if(exists("weather_data")){
      weather_data <- rbind(weather_data,data)
    } else 
      weather_data <- data
  }
  
  setTxtProgressBar(pb, i)
  
}

 close(pb)
 weather_data <- unique(weather_data)
saveRDS(weather_data,'weather_data.rda')