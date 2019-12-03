
#Read in data created from Generator.R
data <- read.csv("~/Stat355/stat355-Project/disney_data_2018.csv")

#Remove NA values
sub <- data[!is.na(data$SPOSTMIN),]

#Seperrate the data categorically by average percipitation (> .13)
sub$precipitation <- 0
sub$precipitation[sub$WEATHER_WDWPRECIP>=mean(sub$WEATHER_WDWPRECIP)] <- 1
less <- sub[sub$precipitation==0,]
more <- sub[sub$precipitation==1,]
#Run t-test
t.test(less$SPOSTMIN, more$SPOSTMIN)

#Create a plot
ggplot(myDF, aes(myDF$SPOSTMIN, myDF$WEATHER_WDWPRECIP)) + geom_bin2d() + 
  scale_fill_gradientn(colours=rainbow(4)) + xlab('Posted Wait Time') + ylab('Precipitation') +
  ggtitle('Heatmap of Posted Wait Times by Precip')

#Look at what the outliers are
sub$rides[sub$SPOSTMIN > 250 & sub$WEATHER_WDWPRECIP > .25]