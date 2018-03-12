# function to plot spatial heat map


plot_spatial <- function(online=FALSE,Date=as.Date("2017-06-01"), measure="t_max") {
  
  onl<- online
  
  # prepare lables for plot
  metrics_desc_map <- readRDS("Metrics.rda")
  metrics_desc_map <- metrics_desc_map[c("id","description")]
  measures <- c("p_official","rh_std","solarad","t_max","t_min","t_official","windspd","ws_max")
  metrics_desc_map <- subset(metrics_desc_map,id %in% measures)
  metrics_desc_map$display_text <- paste0(toupper(metrics_desc_map$id)," [",metrics_desc_map$description,"]")
  
  data <-  getSpatialPlotData(online=onl,date = Date, metric = measure)
  
  #Continental States
  suppressMessages(map <- get_map(location='united states', zoom=4, maptype = "terrain",
                 source='google', color='bw'))
  
  CS<-ggmap(map) + geom_point(data = data, aes(x=longitude, y=latitude, color = value), alpha=0.9, size=5) + 
    scale_color_gradientn(colours =                        
                            c('#a50026','#d73027','#f46d43','#fdae61','#abd9e9','#74add1','#4575b4','#313695'))
  
  #Alaska
  suppressMessages(map<-get_map(location='Alaska', zoom=4, maptype = "terrain",
                                source='google',color='bw'))
 
  AK <- ggmap(map) + geom_point(data = data, aes(x=longitude, y=latitude, color = value), alpha=1, size=5) + 
    scale_color_gradientn(colours =                        
                            c('#a50026','#d73027','#f46d43','#fdae61','#abd9e9','#74add1','#4575b4','#313695'))+
    theme(legend.position = 'none')
  #Hawaii
  suppressMessages(map<-get_map(location='Hawaii', zoom=8, maptype = "terrain",
               source='google',color='bw'))
  HI <- ggmap(map) + geom_point(data = data, aes(x=longitude, y=latitude, color = value), alpha=1, size=5) + 
    scale_color_gradientn(colours =                        
                            c('#a50026','#d73027','#f46d43','#fdae61','#abd9e9','#74add1','#4575b4','#313695'))+
    theme(legend.position = 'none')

  #plotting on map
  options(warn=-1)
  collage <- grid.arrange(CS,AK, HI,layout_matrix = rbind(c(2,2,1,1,1,1), c(3,3,1,1,1,1)),
                          top = textGrob(paste0("Spatial plot:",metrics_desc_map[which(metrics_desc_map$id==measure),c("display_text")]),
                                         gp=gpar(fontsize=20)))
  options(warn=0)            
  return(plot(collage))
  
}
