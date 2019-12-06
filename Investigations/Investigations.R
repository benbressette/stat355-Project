library(dplyr)
library(stringr)
library(data.table)
library(lubridate)
library(tidyr)
library(ggplot2)

#Read in data created from Generator.R
#IMPORTANT!!! Change this value to the location where your csv is
data <- read.csv("~/Stat355/stat355-Project/Data/disney_data_2018.csv")

#This is some data wrangling explained in Question 2
#It is repeated here for some background investigations not related directly
#to the question
data$hour <- hour(data$datetime)
data$SPOSTMIN[is.na(data$SPOSTMIN)] <- -1
data$SPOSTMIN[data$SPOSTMIN > -1] <- 0
closed <- pivot_wider(data, id_cols=c("hour", "date", "WEATHER_WDWPRECIP"), 
                     names_from=rides, values_from=SPOSTMIN, 
                     values_fn=list(SPOSTMIN = min), values_fill=list(SPOSTMIN=0))
closed <- transform(closed, sum = rowSums(closed[,-(1:3)], na.rm=TRUE))
closed$sum <- abs(closed$sum)
closed_day <- closed %>% group_by(date) %>% summarise(closed = mean(sum), precip = min(WEATHER_WDWPRECIP))

summary(closed$sum)

# Ride Waittime Boxplot
ggplot(data, aes(data$rides, data$SPOSTMIN)) + geom_boxplot(aes(col = data$rides)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab('Rides') + ylab('Posted Wait time') + 
  ggtitle('Posted Wait Times by Ride') + theme(legend.position = "none") 

# Histogram of closed rides
ggplot(closed, aes(sum)) + geom_histogram(aes(fill = 'red')) + xlab('Closed Ride Count') + 
  ggtitle('Histogram of Closed Rides')  + theme(legend.position = "none") 

#Group the precipitation by day into a new table
precip_day <- data %>% group_by(date) %>% summarise(precip = min(WEATHER_WDWPRECIP))
#plot the date and precipitation
ggplot(data = precip_day, aes(date, precip)) + geom_point() +
  labs(title = "Precipitation Throughout the Year", xlab = "Date", ylab = "Precipitation (ft)")

