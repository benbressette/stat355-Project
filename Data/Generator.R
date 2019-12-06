library(readxl)
library(dplyr)
library(stringr)
library(data.table)
library(lubridate)
library(tidyr)

#IMPORTANT NOTE:
#I have found that this Generator.R only runs properly on a linux machine (University Scholar Cluster)
#and not on windows. I believe this is because of how each operating system treats files with
#.xlsx extension. Because of this we have included in our github a copy of the csv that this
#R file generates.
#
#We have also found that tripplanner frequently updates their database so if the csv is generated again
#You may get slightly different results than we did.


#Download xlsx file that contains two rows with links to metadata, and 14 rows with links
#To data about each ride
download.file("https://cdn.touringplans.com/datasets/touringplans_data_dictionary.xlsx", "./dis.xlsx")

#IMPORTANT: Change this to your local directory
data <- read_excel("./dis.xlsx")
colnames(data) <- c("links", "content")
#Reads in the data from each link contained in the excel table
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

#Subset the data to only 2018, selects only relevenat columns and outputs to file
slice <- details[details$YEAR == 2018]
slice <- select(slice, date, rides, datetime, SPOSTMIN, SACTMIN, WEATHER_WDWPRECIP, WDWMAXTEMP, WDWMINTEMP, WDWMEANTEMP)
#IMPORTANT: Change the output path to save data to a different directory
fwrite(slice, "./disney_data_2018.csv")

