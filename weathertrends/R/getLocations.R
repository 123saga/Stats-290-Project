getAllLocations <- function(online=TRUE,state=NA){
  
  st <- state
  # save locations information
  
  if(online==TRUE){
    URL <- "https://www.ncdc.noaa.gov/crn/api/v1.0/sites"
    Locations <- fromJSON(RCurl::getURL(URL))
    save(Locations, file='Locations.rda')
    
  } else{
    
    load(file='Locations.rda')
  }
  
  if(st %in% (unique(Locations$state)) | is.na(st)){
    Locations <- Locations%>%
      dplyr::select(state,location,id)
    
    if(!is.na(st)){
      Locations <- Locations%>%
        filter(state==st)
    }
    
    Locations <- unique(Locations)
    
    #return
    Locations
  }else{
    print("Invalid State code, Please enter a valid state code")
  }
  
  
}