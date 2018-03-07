##################################
# adding libraries and database
##################################
library(ggmap)
library(tidyverse)
library(data.table) 
library(tcR)
library(ggplot2)
library(ggExtra)
library(lubridate)


# replace following with weatherdata function
weather_data <- readRDS("C:/Users/viksi/Desktop/Personal/ML/Stats290/Final Project - WeatherData Package/weather_data.rda")
locations <- readRDS("C:/Users/viksi/Desktop/Personal/ML/Stats290/Final Project - WeatherData Package/Locations.rda")

##################################
# Timeseries plots
##################################
plot_tseries <- function(sDate, eDate, measure, siteid){
        
        #  Error handling for Dates
        #  if (sDate >= eDate) {
        #          print("End Date must be after Start Date")
        #          
        #  }
        
        # Error handling for INVALID dates, measure, site id
        # Error handling for not entering a variable
        
        data <-   weather_data %>% 
                filter(metric==measure, id==siteid) %>% 
                separate(col=time, into=c("date","time"), sep="T") %>%
                group_by(date) %>%
                summarize(value=max(value)) 
        
        sIndex <- which(data$date==sDate)
        eIndex <- which(data$date==eDate)
        data <- slice(data, sIndex:eIndex)
        
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

##################################
# Box Plots
##################################
plot_boxplot <- function(sDate, eDate, measure, siteid){
        
        # Same error handling as above over here
        # once everything stiches well, abstract all these 4 function with one plot() 
        
        sD <- paste0(sDate,"T00:00:00Z") 
        eD <- paste0(eDate,"T00:00:00Z") 
        
        data <- weather_data %>% 
                filter(metric==measure, id==siteid) %>% 
                select(time, value) 
        sIndex <- which(data$time==sD)
        eIndex <- which(data$time==eD)
        
        data <- slice(data, sIndex:eIndex)
        
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
        
        print("5 most important quantiles")
        print(quantile(data$value, c(0,0.25,0.5,0.75,1)))
        
        plot <-  ggplot(data = data, aes(x = "", y = value)) + 
                geom_boxplot(fill="#4271AE", colour="#1F3552", size=1.5, alpha = 0.7)+
                theme_minimal(base_size = 20)+
                theme(legend.position = "none")+
                ggtitle(label="Boxplot of Weather Indicator")+
                labs(x=NULL, y=y_axis_label)
        plot      
        # examples to try
        # plot_boxplot("2017-03-02", "2017-03-24", "rh_std", 1001)        
}

##################################
# Calendar Heat Map
##################################
plot_calhmap <- function(sDate, eDate, measure, siteid, text=TRUE){
        
        print("Advice: Pass text=FALSE if difference between start and end date is more than 2 months")
        
        #selecting date for specific siteid
        data <- weather_data %>% 
                filter(id==siteid) %>% 
                select(time, metric, value) 
        
        #making sure dplyr is loaded after plyr to avoid issues with summarizing()
        
        #selecting date for specific measure
        data <- separate(data, col=time, into=c("date","time"), sep="T") %>%
                select(date, metric, value) %>%
                group_by(metric, date) %>%
                summarize(value=max(value)) %>%
                filter(metric==measure)
        
        
        # selecting data for specific start and end date
        sIndex <- which(data$date==as.Date(sDate))
        eIndex <- which(data$date==as.Date(eDate))
        data <- slice(data, sIndex:eIndex)
        
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
                ggtitle(label="Heatmap of Weather Indicators",subtitle = "# Activity per day")+
                labs(x="Week Beginning", y=NULL) +
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

##################################
#Spatial Maps
##################################
plot_spatial <- function(Date=as.Date("2017-06-01"), measure="t_max") {
        
        data <- weather_data %>% 
                filter(metric==measure) %>%
                separate(col=time, into=c("date","time"), sep="T") %>%
                filter(date==Date) %>%
                group_by(id) %>%
                summarize(value=max(value)) %>%
                select(value, id)
        
        locate <- select(locations, id, latitude, longitude, state, location)
        
        data <- inner_join(data, locate, by="id")
        
        data_CS <- filter(data, state != "AK" & state != "HI")
        data_AK <- filter(data, state == "AK")
        data_HI <- filter(data, state == "HI")
        
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
                                              c('#a50026','#d73027','#f46d43','#fdae61','#abd9e9','#74add1','#4575b4','#313695'))
        #Hawaii
        map<-get_map(location='Hawaii', zoom=8, maptype = "terrain",
                     source='google',color='bw')
        HI <- ggmap(map) + geom_point(data = data, aes(x=longitude, y=latitude, color = value), alpha=1, size=5) + 
                scale_color_gradientn(colours =                        
                                              c('#a50026','#d73027','#f46d43','#fdae61','#abd9e9','#74add1','#4575b4','#313695'))
        
        options(warn=-1)
        return(gridExtra::grid.arrange(CS, AK, HI, nrow=2))
        options(warn=0)
        
        #example to try
        #plot_spatial()
}
