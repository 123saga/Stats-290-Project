plot_calhmap <- function(online=FALSE,from='2017-10-01', to='2017-10-10', measure='t_max', location='Wolf Point',state='MT', text=TRUE){
  
  print("Advice: Pass text=FALSE if difference between start and end date is more than 2 months")
  
  sdate <- as.Date(from)
  edate <- as.Date(to)
  loc <- as.character(location)
  st <- as.character(state)
  measure <- as.character(measure)
  onl <- online
  
  # prepare lables for plot
  metrics_desc_map <- getMetrics()
  metrics_desc_map <- metrics_desc_map[c("id","description","units")]
  measures <- c("p_official","rh_std","solarad","t_max","t_min","t_official","windspd","ws_max")
  metrics_desc_map <- subset(metrics_desc_map,id %in% measures)
  metrics_desc_map$display_text <- paste0(toupper(metrics_desc_map$id)," [",metrics_desc_map$description,"]")
  
  plot_title <- paste0("Heat Map for:",metrics_desc_map[which(metrics_desc_map$id==measure),c("display_text")])
  
  ## function to get weather a data by location, all other params are optional
  data <- getWeatherData(online=onl,
                         location=loc,state=st,
                         from=sdate,
                         to=edate)
  
  # filter for selected params
  data <- separate(data, col=time, into=c("date","time"), sep="T") %>%
    select(date, metric, value) %>%
    group_by(metric, date) %>%
    summarize(value=max(value)) %>%
    filter(metric==measure)
  

  # prepating data fro plotting
  data$date <- as.Date(data$date)
  data$metric <- as.factor(data$metric)
  data$value <- as.numeric(data$value)
  
  data <- select(data, date, metric, value)
  
  #getting data ready for plot
  data$day = lubridate::wday(data$date,label=TRUE) 
  data$day = with(data, factor(day, levels = rev(levels(day)))) 
  data$weekStart = data$date - as.POSIXlt(data$date)$wday 
  
  #rescaling the value 
  data = data %>% group_by(metric)%>% 
    mutate(rescaled_value = scales::rescale(value))
  
  # plotting the heatmap
  plot<-  ggplot(data,aes(x=weekStart, y=day, fill=value))+ 
    geom_tile(colour="white",size=.1) + 
    scale_fill_gradient(high="red",low="yellow") +
    scale_x_date(breaks=unique(data$weekStart),date_labels="%d-%b-%y")+
    theme_minimal(base_size = 20)+
    removeGrid()+
    rotateTextX()+
    ggtitle(label=plot_title)+
    labs(x="Calender Week Day (Sunday)", y=NULL) +
    theme(
      legend.position="none"
    )
  
  if(text==TRUE){
    plot+
      geom_text(data=data,aes(weekStart,day,label=value),colour="black",size=6)
  } else {
    plot
  }
  
  #example to test
  #plot_calhmap("2017-01-02","2017-02-04","t_official",1001, TRUE)
}