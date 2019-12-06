library(readxl)
library(dplyr)
library(stringr)
library(data.table)
library(lubridate)
library(tidyr)

#Read in data created from Generator.R
#IMPORTANT: Change this to the location where your csv is stored
data <- read.csv("~/Stat355/stat355-Project/Data/disney_data_2018.csv")

#Add hour column
data$hour <- hour(data$datetime)
closed <- data
#Replace all NA (ride closed) values with -1. Then replace all values greater than -1 with 0
#This will later allow us to count number of rides closed
closed$SPOSTMIN[is.na(closed$SPOSTMIN)] <- -1
closed$SPOSTMIN[closed$SPOSTMIN > -1] <- 0

#Create Average number of rides closed pivot
closed <- pivot_wider(closed, id_cols=c("hour", "date"), 
                     names_from=rides, values_from=SPOSTMIN, 
                     values_fn=list(SPOSTMIN = min), values_fill=list(SPOSTMIN=0))
#Add a column with the sum of rides closed
closed <- transform(closed, sum = rowSums(closed[,-(1:3)], na.rm=TRUE))
closed$sum <- abs(closed$sum)

#Summarise the closed table to get the average amount of rides closed throughout each day
closed <- closed %>% group_by(date) %>% summarise(closed = mean(sum))

#Create table of average total wait time per day
daily <- pivot_wider(data, id_cols=c("hour", "date"), 
                     names_from=rides, values_from=SPOSTMIN, 
                     values_fn=list(SPOSTMIN = min))
#Add a row with the average wait time of all rides
daily <- transform(daily, average = rowMeans(daily[,-2], na.rm=TRUE))
daily <- daily %>% group_by(date) %>% summarize(average = mean(average, na.rm=TRUE))

#Merge the table of average rides closed with the table of average wait time by date
daily <- merge(x=daily, y=closed, by="date")

#Standardize the columns
daily$std_wait <- scale(daily$average)
daily$std_closed <- scale(daily$closed)

#Run Correlation
cor(daily$std_wait, daily$std_closed)
plot(daily$closed, daily$average, 
     main="Daily Wait Time vs Daily Ride Closures", 
     xlab="Average Rides Closed During Day", ylab="Average Wait Time Throughout Day")
