getAllLocations <- function(online=FALSE){
  
  if(online==TRUE){
    URL <- "https://www.ncdc.noaa.gov/crn/api/v1.0/sites"
    
    Locations <- fromJSON(RCurl::getURL(URL))
  } else{
    
    Locations <- readRDS("Distance_data_master.rda")
  }
    
    Locations <- Locations%>%
      dplyr::select(state,location)
    
    Locations <- unique(Locations)             
  
  
  #return
  Locations
}