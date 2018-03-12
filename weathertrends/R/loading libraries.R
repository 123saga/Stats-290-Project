## Install the package if needed
installIfNeeded <- function(packages, ...) {
        for (package in packages) {
                installedPackages <- installed.packages()[, 1]
                if (! (package %in% installedPackages))
                        suppressMessages(install.packages(package, ...))
        }
}
installIfNeeded(c("RCurl","jsonlite","tidyverse","lubridate","ggplot2","ggmap","ggExtra","gridExtra","grid"))


# loading libraries
library(RCurl)
library(jsonlite)

library(tidyverse)

# date libs
library(lubridate)

# plot libs
library(ggplot2)
library(ggmap)
library(ggExtra)
library(gridExtra)
library(grid)
