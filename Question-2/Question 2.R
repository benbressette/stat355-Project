library(dplyr)
library(stringr)
library(data.table)
library(lubridate)
library(tidyr)
library(ggplot2)

#Read in data created from Generator.R
data <- read.csv("~/Stat355/stat355-Project/Data/disney_data_2018.csv")

#Add hour column
data$hour <- hour(data$datetime)
data$SPOSTMIN[is.na(data$SPOSTMIN)] <- -1
data$SPOSTMIN[data$SPOSTMIN > -1] <- 0

#Pivot table based on hour and date, this pivot is to standardize the time interval of the data
closed <- pivot_wider(data, id_cols=c("hour", "date", "WEATHER_WDWPRECIP"), 
                     names_from=rides, values_from=SPOSTMIN, 
                     values_fn=list(SPOSTMIN = min), values_fill=list(SPOSTMIN=0))
#Sum the total number of rides closed for each hour.
closed <- transform(closed, sum = rowSums(closed[,-(1:3)], na.rm=TRUE))
closed$sum <- abs(closed$sum)
#Get the average amount of rides closed throughout each day
closed <- closed %>% group_by(date) %>% summarise(closed = mean(sum), precip = min(WEATHER_WDWPRECIP))
#Generate linear regression for number of rides closed vs precipitation
reg <- lm(closed ~ precip, closed)
reg

#Plot the number of rides closed vs precipitation, with the regression line
plot(closed ~ precip, data = closed, main = '# of Rides Closed by Precipitation', 
     xlab = 'Precipitation (ft)', ylab = 'Average Closed Rides')
abline(a=coef(reg)[1], b=coef(reg)[2], col=2, lwd=3)

#Generate plots to verify linear regression is a good fit
plot(reg)





