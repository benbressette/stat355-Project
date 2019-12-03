library(readxl)
library(dplyr)
library(stringr)
library(data.table)
library(lubridate)
library(tidyr)

#Read in data created from Generator.R
data <- read.csv("~/Stat355/stat355-Project/disney_data_2018.csv")

#Add hour column
data$hour <- hour(data$datetime)
data$SPOSTMIN[is.na(data$SPOSTMIN)] <- -1
data$SPOSTMIN[data$SPOSTMIN > -1] <- 0

pivot <- pivot_wider(data, id_cols=c("hour", "date", "WEATHER_WDWPRECIP"), 
                     names_from=rides, values_from=SPOSTMIN, 
                     values_fn=list(SPOSTMIN = min), values_fill=list(SPOSTMIN=0))
closed <- pivot
closed
closed <- transform(closed, sum = rowSums(closed[,-(1:3)], na.rm=TRUE))
closed$sum <- abs(closed$sum)

closed_day <- closed %>% group_by(date) %>% summarise(closed = mean(sum), precip = min(WEATHER_WDWPRECIP))

reg <- lm(closed ~ precip, closed_day)
plot(reg)

