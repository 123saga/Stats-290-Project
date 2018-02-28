library(geosphere)
setwd("C:/Users/gsgr/Documents/SCPD/STATS290/Project/Stats-290-Project")
load('database.rda')

Locations <- unique(allfiles[,c("AREA","STATE","LONGITUDE","LATITUDE")])

for(i in 1:nrow(Locations)){
  ## get current location
  Current_location <- Locations[c(i),]
  ## get rest of the location
  Rest_of_location <- Locations[-c(i),]
  
  for(j in 1:nrow(Rest_of_location)){
    Rest_of_location$Distance[j] <- distm(c(Current_location$LONGITUDE, Current_location$LATITUDE),
                                          c(Rest_of_location$LONGITUDE[j], Rest_of_location$LATITUDE[j]),
                                          fun = distHaversine)
    
  }
  names(Rest_of_location) <- paste0("TO_",names(Rest_of_location))
  
  Distance_data <- merge(as.data.frame(Current_location),
                         as.data.frame(Rest_of_location),
                         by=NULL)
  
  if(exists("Distance_data_master")){
  Distance_data_master <- rbind(Distance_data_master,Distance_data)
  } else 
    Distance_data_master <- Distance_data
  }