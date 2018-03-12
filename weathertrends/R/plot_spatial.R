


plot_spatial <- function(online=FALSE,Date=as.Date("2017-06-01"), measure="t_max") {
  
  onl<- online
  
  data <-  getSpatialPlotData(online=onl,date = Date, metric = measure)
  
  #Continental States
  map <- get_map(location='united states', zoom=4, maptype = "terrain",
                 source='google', color='bw')
  
  CS<-ggmap(map) + geom_point(data = data, aes(x=longitude, y=latitude, color = value), alpha=0.9, size=5) + 
    scale_color_gradientn(colours =                        
                            c('#a50026','#d73027','#f46d43','#fdae61','#abd9e9','#74add1','#4575b4','#313695'))
  
  #Alaska
  map<-get_map(location='Alaska', zoom=4, maptype = "terrain",
               source='google',color='bw')
  AK <- ggmap(map) + geom_point(data = data, aes(x=longitude, y=latitude, color = value), alpha=1, size=5) + 
    scale_color_gradientn(colours =                        
                            c('#a50026','#d73027','#f46d43','#fdae61','#abd9e9','#74add1','#4575b4','#313695'))+
    theme(legend.position = 'none')
  #Hawaii
  map<-get_map(location='Hawaii', zoom=8, maptype = "terrain",
               source='google',color='bw')
  HI <- ggmap(map) + geom_point(data = data, aes(x=longitude, y=latitude, color = value), alpha=1, size=5) + 
    scale_color_gradientn(colours =                        
                            c('#a50026','#d73027','#f46d43','#fdae61','#abd9e9','#74add1','#4575b4','#313695'))+
    theme(legend.position = 'none')

  #plotting on map
  options(warn=-1)
  collage <- grid.arrange(CS,AK, HI,layout_matrix = rbind(c(2,2,1,1,1,1), c(3,3,1,1,1,1)), top = paste0("Spatial plot for:",measure))
  options(warn=0)            
  return(collage)
  
  #example to try
  #plot_spatial()
}
