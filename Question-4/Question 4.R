# Histogram of closed rides
ggplot(closed, aes(sum)) + geom_histogram(aes(fill = 'red'))