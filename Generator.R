library(readxl)
library(dplyr)
library(stringr)
library(data.table)
library(lubridate)
library(tidyr)
options(error=recover)
download.file("https://cdn.touringplans.com/datasets/touringplans_data_dictionary.xlsx", "./dis.xlsx")

data <- read_excel("./dis.xlsx")
colnames(data) <- c("links", "content")
data$data <- sapply(data$links, function(x)fread(str_trim(x), na.strings="-999"))

#Manually sets all rows to 'ride', then the first 2 to NA
data$type = "ride"
data$type[1] = NA
data$type[2] = NA

#Uses regex to extract name from link
data$ride <- sapply(data$links, function(x)str_match(x, "([^\\/.]+)\\.[^.]*$")[2])

#Updates the names of the data.tables to the ride names
names(data$data) <- data$ride
names(data$data)
#Subsets just rides/metadata
rides <- data[!is.na(data$type),]
meta <- data[is.na(data$type),]

#Uses rbindlist to create a new data frame with all the rows from the rides
details <- rbindlist(rides$data, use.names = T, idcol="rides")
#Merges the rows from rides with the metadata based on the date
details <- merge(x=details, y=meta$data[[1]], by.x="date", by.y="DATE", all.x=TRUE, all.y=FALSE, allow.cartesian=TRUE)
#Format Date
details$date <- mdy(details$date)

slice <- details[details$YEAR == 2018]
slice <- select(slice, date, rides, datetime, SPOSTMIN, SACTMIN, WEATHER_WDWPRECIP, WDWMAXTEMP, WDWMINTEMP, WDWMEANTEMP)

fwrite(slice, "disney_data_2018.csv")
