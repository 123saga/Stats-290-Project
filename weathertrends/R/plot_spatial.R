library(ggplot2)
library(ggmap)
library(gridExtra)
library(RCurl)
library(jsonlite)



plot_spatial <- function(online=FALSE,Date=as.Date("2017-06-01"), measure="t_max") {
  
  onl<- online
  
  data <-  getPlotData(online=onl,date = Date, metric = measure)
  
  # data_CS <- filter(data, state != "AK" & state != "HI")
  # data_AK <- filter(data, state == "AK")
  # data_HI <- filter(data, state == "HI")
  
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


  
  # options(warn=-1)
  # return(gridExtra::grid.arrange(CS, AK, HI, nrow=2))
  # options(warn=0)

  #collage <- grid.arrange(CS,AK,HI,layout_matrix = rbind(c(1,1,1,1), c(2,2,3,3)), top = "Main title", bottom="Three plots")
  collage <- grid.arrange(CS,AK, HI,layout_matrix = rbind(c(2,2,1,1,1,1), c(3,3,1,1,1,1)), top = paste0("Spatial plot for:",measure))
               
  return(collage)
  
  #example to try
  #plot_spatial()
}
