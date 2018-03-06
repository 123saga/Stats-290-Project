library(tidyverse)
library(data.table)
library(zoo)
library(lubridate)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
options(scipen = 999)
setwd("C:/Users/gsgr/Documents/SCPD/STATS290/Project/Stats-290-Project")


getAllLocations <- function(){
  
  Locations <- readRDS("Distance_data_master.rda")
  
  Locations <- Locations%>%
    dplyr::select(state,location)
  
  Locations <- unique(Locations)             
  
  #return
  Locations
}