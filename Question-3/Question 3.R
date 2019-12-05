# !diagnostics off
library(readxl)
library(dplyr)
library(stringr)
library(data.table)
library(lubridate)
library(tidyr)

#Read in data created from Generator.R
data <- read.csv("~/Stat355/stat355-Project/Data/disney_data_2018.csv")

#Add hour column
data$hour <- hour(closed$datetime)
closed <- data
closed$SPOSTMIN[is.na(closed$SPOSTMIN)] <- -1
closed$SPOSTMIN[closed$SPOSTMIN > -1] <- 0

#Create Average number of rides closed pivot
closed <- pivot_wider(closed, id_cols=c("hour", "date"), 
                     names_from=rides, values_from=SPOSTMIN, 
                     values_fn=list(SPOSTMIN = min), values_fill=list(SPOSTMIN=0))
closed <- transform(closed, sum = rowSums(closed[,-(1:3)], na.rm=TRUE))
closed$sum <- abs(closed$sum)

closed <- closed %>% group_by(date) %>% summarise(closed = mean(sum))

#Create table of average total wait time per day
daily <- pivot_wider(data, id_cols=c("hour", "date"), 
                     names_from=rides, values_from=SPOSTMIN, 
                     values_fn=list(SPOSTMIN = min))
daily <- transform(daily, average = rowMeans(daily[,-2], na.rm=TRUE))
daily <- daily %>% group_by(date) %>% summarize(average = mean(average, na.rm=TRUE))
daily <- merge(x=daily, y=closed, by="date")

#Standardize the columns
daily$std_wait <- scale(daily$average)
daily$std_closed <- scale(daily$closed)

#Run Correlation
cor(daily$std_wait, daily$std_closed)
plot(daily$closed, daily$average, 
     main="Daily Wait Time vs Daily Ride Closures", 
     xlab="Aver Rides Closed During Day", ylab="Average Wait Time Throughout Day")
