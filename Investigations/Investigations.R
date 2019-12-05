pivot <- pivot_wider(data, id_cols=c("hour", "date", "WEATHER_WDWPRECIP"), 
                     names_from=rides, values_from=SPOSTMIN, 
                     values_fn=list(SPOSTMIN = min), values_fill=list(SPOSTMIN=0))
closed <- pivot
closed
closed <- transform(closed, sum = rowSums(closed[,-(1:3)], na.rm=TRUE))
closed$sum <- abs(closed$sum)

closed_day <- closed %>% group_by(date) %>% summarise(closed = mean(sum), precip = min(WEATHER_WDWPRECIP))


summary(closed$sum)

# Ride Waittime Boxplot
ggplot(myDF, aes(myDF$rides, myDF$SPOSTMIN)) + geom_boxplot(aes(col = myDF$rides)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab('Rides') + ylab('Posted Wait time') + 
  ggtitle('Posted Wait Times by Ride') + theme(legend.position = "none") 

