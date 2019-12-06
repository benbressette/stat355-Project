library(dplyr)
library(stringr)
library(data.table)
library(lubridate)
library(tidyr)

#Read in data created from Generator.R
#IMPORTANT: Change this to the location you saved the csv
data <- read.csv("~/Stat355/stat355-Project/Data/disney_data_2018.csv")

#Remove NA values
sub <- data[!is.na(data$SPOSTMIN),]

#Seperrate the data categorically by average percipitation (> .13)
sub$temp <- 0
sub$temp[sub$WDWMEANTEMP>=mean(sub$WDWMEANTEMP)] <- 1
less <- sub[sub$temp==0,]
more <- sub[sub$temp==1,]
#Run t-test
t.test(less$SPOSTMIN, more$SPOSTMIN)


