library(tidyverse)
library(data.table)
library(zoo)
library(lubridate)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
options(scipen = 999)
setwd("C:/Users/gsgr/Documents/SCPD/STATS290/Project/Stats-290-Project")

## function to get weather a data by location, all other params are optional
getWeatherData  <- function(online=TRUE,
                                        location,
                                        state,
                                        from="2017-01-01",
                                        to="2017-01-31",
                                        #metrics= NA,
                                        range=NA)
  {
  
  # Implement try-cath block to check inputs
  # format inputs
  from <- as.Date(from)
  to <- as.Date(to)
  loc <- trim(location)
  st <- trim(state)
  dist <- as.numeric(range)
  location_ids <- NA
  
  if(is.na(location) || is.na(state) || state==""  || location==""){
    print("Please enter valid location details")
    
  } else {
    
    Locations <- readRDS("Distance_data_master.rda")
    
    if(is.na(dist)){
      ## get id for selected location
      location_ids <- Locations%>%
        filter(state==st,location==loc)%>%
        dplyr::select(id)
      
      location_ids <- unique(location_ids)
      
    } else {
      
      # convert miles to meters
      dist <- 1609.34*dist
      
      # get id for selected state and location
      location_ids <- Locations%>%
        filter(state==st,location==loc)%>%
        dplyr::select(id)
      
      location_ids <- unique(location_ids)
      
      # get remaining ids that fall under range from area, id entered
      location_ids2 <- Locations%>%
        filter(id %in% location_ids,TO_Distance<dist)%>%
        dplyr::select(TO_id)
      
      location_ids2 <- unique(location_ids2)
      
      location_ids <- rbind(location_ids,location_ids2)
    } 
    
    if(nrow(location_ids)==0){
      
      print("No data avaiable for given input parameters, please check the values once again")
      
    }else{
    
    # check offline falg
    if(online==FALSE){
      ## connect to rda file
      weather_data <- readRDS("weather_data.rda")
      
      weather_data <- weather_data%>%
        filter(id %in% location_ids$id)%>%
        filter(time >= from & time <= to)
      
      ## return data
      if(nrow(weather_data)==0){
        print("No data avaiable for given input parameters, please check the values once again")
      }
      weather_data
      
    }else {
      
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
          } else{
            weather_data <- data
          }
        }
        
        
      }
      ## return data
      if(!exists("weather_data")){
        print("No data avaiable for given input parameters, please check the values once again")
      }
      weather_data
    } 
      } # end locations validation 
  }# end of input verification
}