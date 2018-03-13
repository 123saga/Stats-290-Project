plot_boxplot <- function(online=FALSE, from='2017-10-01', to='2017-10-10', measure='t_max', location='Wolf Point',state='MT'){
  
  # Same error handling as above over here
  # once everything stiches well, abstract all these 4 function with one plot() 
  
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
  
  plot_title <- paste0("Box plot:",metrics_desc_map[which(metrics_desc_map$id==measure),c("display_text")])
  
  ## function to get weather a data by location, all other params are optional
  data <- getWeatherData(online=onl,
                         location=loc,state=st,
                         from=sdate,
                         to=edate)
  
  
  data <-   data%>% 
    filter(metric==measure) %>% 
    separate(col=time, into=c("date","time"), sep="T") %>%
    group_by(date) %>%
    summarize(value=max(value))
  
  data$date <- as.Date(data$date)

  
  # Print units on y axis depending on measure
  if (measure=="t_official" | measure=="t_max" | measure=="tmin"){
    y_axis_label = "Temperature (Celcius)"
  } else if (measure=="p_official") {
    y_axis_label = "Precipitation (mm)" 
  } else if (measure=="windspd" | measure=="ws_max") {
    y_axis_label = "Wind Speed (m/s)" 
  } else if (measure=="rh_std") {
    y_axis_label = "Relative Humidity %"
  } else if (measure=="solarad"){
    y_axis_label = "Solar Radiation (W/m^2)" 
  }
  
  
  
  print("5 most important quantiles")
  print(quantile(data$value, c(0,0.25,0.5,0.75,1)))
  
  plot <-  ggplot(data = data, aes(x = "", y = value)) + 
    geom_boxplot(fill="#4271AE", colour="#1F3552", size=1.5, alpha = 0.7)+
    theme_minimal(base_size = 20)+
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
    ggtitle(label=plot_title)+
    labs(x=NULL, y=y_axis_label)
  
  plot      
}
