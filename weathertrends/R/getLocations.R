getAllLocations <- function(){
  
  Locations <- readRDS("Distance_data_master.rda")
  
  Locations <- Locations%>%
    dplyr::select(state,location)
  
  Locations <- unique(Locations)             
  
  #return
  Locations
}