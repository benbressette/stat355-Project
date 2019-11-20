library(readxl)
library(dplyr)
library(stringr)
library(data.table)
library(lubridate)
library(tidyr)
library(ggplot2)
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

#Sets the non-ride names to NA 
#(personally I think it would have been better to keep the name as metadata/entities)
data <- data %>%
  mutate(ride = case_when(is.na(type) ~ NA, TRUE ~ ride))

#Updates the names of the data.tables to the ride names
names(data$data) <- data$ride

#Subsets just rides/metadata
rides <- data[!is.na(data$type),]
meta <- data[is.na(data$type),]

#Uses rbindlist to create a new data frame with all the rows from the rides
details <- rbindlist(rides$data, use.names = T, idcol="rides")
#Merges the rows from rides with the metadata based on the date
details <- merge(x=details, y=meta$data[[1]], by.x="date", by.y="DATE")
#Replace NA
details$SPOSTMIN[is.na(details$SPOSTMIN)] <- 0
#Format Date
details$date <- mdy(details$date)


#Q2
details$day_of_week <- wday(details$date, label=T)
unique(details$day_of_week)

#Create pivot table
times <- pivot_wider(details, id_cols=c("day_of_week", "YEAR"), 
            names_from=rides, values_from=SPOSTMIN, values_fn=list(SPOSTMIN = mean))
times <- setDT(times)
#Remove NA values from pivot table
times[is.na(times)] <- 0
#Create new column with name of max ride
times[, max := colnames(.SD)[max.col(.SD, ties.method = "first")], .SDcols=rides$ride]
#Print out just the day of week, year, and most popular ride
times %>% select(day_of_week, YEAR, max)

weather <- details
#Add Precipitation category
weather$precipitation <- "Less"
weather$precipitation[weather$WEATHER_WDWPRECIP>=.2] <- "More"
#Normalize Data
weather[, nwait:=list((SPOSTMIN-min(SPOSTMIN))/(max(SPOSTMIN)-min(SPOSTMIN)))]
weather_pivot <- pivot_wider(weather, id_cols=rides, 
            names_from=precipitation, values_from=nwait, values_fn=list(nwait = mean))
setDT(weather_pivot)
#weather[, c("Less", "More"):=list((Less-min(Less))/(max(Less)-min(Less)),(More-min(More))/(max(More)-min(More)))]
weather_pivot$Diff <- weather_pivot$Less-weather_pivot$More
weather_pivot
#From the difference, splash_mountain has an increase in popularity. 


#Q3
mountain <- details[details$rides == "splash_mountain"]
#Create new hour column, using lubridate
mountain$hour <- hour(mountain$datetime)
#Pivot on day of week and hour
mountain_pivot <- pivot_wider(mountain, id_cols=c("hour", "day_of_week"), 
                     names_from=rides, values_from=SPOSTMIN, values_fn=list(SPOSTMIN = mean))
mountain_pivot <- setDT(mountain_pivot)
mountain_pivot

#Create ggplot
p <- ggplot(mountain_pivot, aes(hour, splash_mountain)) + 
  geom_line() + facet_grid(cols = vars(day_of_week)) + labs(x="Time", y="Average Wait time") +
  theme(axis.text.x = element_text(angle=75, hjust = 1),
          strip.text = element_text(size = 13))
p

#Holliday Masks - from assignment
newyear <- seq(mdy('01/01/2012'), mdy('01/01/2019'), by = '1 year')
christmaseve <- seq(mdy('12/24/2012'), mdy('12/24/2019'), by = '1 year')
christmas <- seq(mdy('12/25/2012'), mdy('12/25/2019'), by = '1 year')
halloween <- seq(mdy('10/31/2012'), mdy('10/31/2019'), by = '1 year')
independence <- seq(mdy('07/04/2012'), mdy('07/04/2019'), by = '1 year')
holidays <- c(newyear, christmaseve, christmas, halloween, independence)

#Filter splash_mountain with holiday dates
mountain$holiday <- mountain$date %in% holidays
mountain_h_pivot <- pivot_wider(mountain, id_cols=c("hour", "day_of_week", "holiday"), 
                              names_from=rides, values_from=SPOSTMIN, values_fn=list(SPOSTMIN = mean))
mountain_h_pivot <- setDT(mountain_h_pivot)
mountain_h_pivot

p <- ggplot(mountain_h_pivot, aes(hour, splash_mountain, colour = holiday, group = holiday)) + 
  geom_line() + facet_grid(cols = vars(day_of_week)) + labs(x="Time", y="Average Wait time") +
  theme(axis.text.x = element_text(angle=75, hjust = 1),
        strip.text = element_text(size = 13))
p
