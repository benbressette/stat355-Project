# Histogram of closed rides
ggplot(closed, aes(sum)) + geom_histogram(aes(fill = 'red')) + xlab('Closed Ride Count') + 
  ggtitle('Histogram of Closed Rides')  + theme(legend.position = "none") 
