getAllLocations <- function(online=FALSE,state="CA"){
  
  st <- state
  
  if(online==TRUE){
    URL <- "https://www.ncdc.noaa.gov/crn/api/v1.0/sites"
    
    Locations <- fromJSON(RCurl::getURL(URL))
  } else{
    
    Locations <- readRDS("Locations.rda")
  }
    
    if(st %in% (unique(Locations$state))){
    Locations <- Locations%>%
      dplyr::select(state,location)%>%
      filter(state==st)
    
    Locations <- unique(Locations)
    
    #return
    Locations
    }else{
      print("Invalid State code, Please enter a valid state code")
    }
  

}