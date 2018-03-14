# function to get weather data by "location" and "state"; all other params are optional
getWeatherData  <- function(online=TRUE,
                            location,
                            state,
                            from="2017-01-01",
                            to="2017-01-02",
                            range=NA) 
{
  
  
  
  # formatting inputs
  from <- as.Date(from)
  to <- as.Date(to)
  loc <- trim(location)
  st <- trim(state)
  dist <- as.numeric(range)
  location_ids <- NA
  
  out <- tryCatch(
    {
      # checking for inavailability of mandatory params
      if(is.na(location) || is.na(state) || state==""  || location==""){
        print("Please pass required 'location' and 'state' parameters") 
        
      } else {
        
        load(file="Distance_data_master.rda")
        Locations <- Distance_data_master
        rm(Distance_data_master)
        
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
          names(location_ids2) <- c("id")
          
          location_ids <- rbind(location_ids,location_ids2)
        } 
        
        if(nrow(location_ids)==0){
          
          print("No data available for given input parameters, please check the values once again. Use getAllLocations() to view valid location and state options")
          return(NULL)
          
        }else{
          
          # check online flag
          if(online==FALSE){
            ## connect to rda file
            load(file="weather_data.rda")
            
            weather_data <- weather_data%>%
              filter(id %in% location_ids$id)%>%
              filter(time >= from & time <= to)%>%
              filter(flag==0)
              
            
            ## return data
            if(nrow(weather_data)==0){
              print("No data avaiable for given input parameters, please check the values once again")
              return(NULL)
            }
            weather_data<- weather_data%>%
              inner_join((unique(Locations%>%select(id,location,vector,state))),by="id")
            
            weather_data
            
          }else {
            
            ## make API call
            to <- as.Date(to)+days(1)
            for (i in 1:nrow(location_ids)){
              loc_id<-location_ids$id[i]
              API_URL <- "https://www.ncdc.noaa.gov/crn/api/v1.0/sites/"
              API_URL_final<- paste0(API_URL,loc_id,
                                     "/data?start=",from,"T00:00Z&end=",to,
                                     "T00:00Z&metric=t_official&metric=t_max&metric=t_min&metric=ws_max",
                                     "&metric=windspd&metric=rh_std&metric=solarad&metric=p_official")
              
              
              data <- fromJSON(RCurl::getURL(API_URL_final))
              
              if(is.data.frame(data)){
                data <- data[,c("start","value","metric","flag")]
                names(data) <- c("time","value","metric","flag")
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
              return(NULL)
            }else{
              weather_data<- weather_data%>%
                filter(flag==0)%>%
                inner_join((unique(Locations%>%select(id,location,vector,state))),by="id")
              
              # return weather_data
              weather_data
              
            }
            
          } 
        } # end locations validation 
      }# end of input verification
    },
    error=function(cond) {
      message(cond)
      return(NA)
    }
  )
  return(out)
  
}