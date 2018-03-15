# function to get list of metrics and definations

getMetrics <- function(online=TRUE){
  
  if(online==TRUE){
    URL <- "https://www.ncdc.noaa.gov/crn/api/v1.0/metrics"
    Metrics <- fromJSON(RCurl::getURL(URL))
    save(Metrics, file='Metrics.rda')
    
  } else{
    
    load(file="Metrics.rda")
  }
  
  #return
  Metrics

  
}