library(tcR)
library(ggExtra)

plot_tseries <- function(online=FALSE,from='2017-10-01', to='2017-10-10', measure='t_max', location='Wolf Point',state='MT'){
  
  #  Error handling for Dates
  #  if (sDate >= eDate) {
  #          print("End Date must be after Start Date")
  #          
  #  }
  
  # Error handling for INVALID dates, measure, site id
  # Error handling for not entering a variable
  
  sdate <- as.Date(from)
  edate <- as.Date(to)
  loc <- as.character(location)
  st <- as.character(state)
  measure <- as.character(measure)
  onl <- online
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
    y_axis_label = "Temp in Celcius"
  } else if (measure=="p_official") {
    y_axis_label = "Precipitation in mm" 
  } else if (measure=="windspd" | measure=="ws_max") {
    y_axis_label = "Wind Speed in m/s" 
  } else if (measure=="rh_std") {
    y_axis_label = "Relative Humidity in %"
  } else if (measure=="solarad"){
    y_axis_label = "Solar Radiation in W/m^2" 
  }
  
  
  plot <-  ggplot(data) + 
    aes(x=date, y=value) + 
    geom_line(colour="blue", size=1) +
    removeGrid() +
    scale_x_date(date_breaks = "1 week", date_labels="%d-%b-%y") +
    theme_minimal(base_size = 20) +
    rotateTextX() +
    ggtitle(label="Time Series Plot of Weather Indicator") +
    labs(x=NULL, y=y_axis_label)
  
  plot 
  # examples to try
  # plot_tseries("2017-01-02", "2017-03-04", "rh_std", 1001)
  # plot_tseries("2017-03-02", "2017-03-24", "rh_std", 1001)
}
