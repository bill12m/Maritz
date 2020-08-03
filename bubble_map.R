cat("\014")
library(ggplot2)
library(maps)
usa <- map_data("usa")
#state_count <- arrange(state_count,State)
state_count <- state_count[-1,]
write.csv(state_count,'states.csv')

ggplot() + 
  geom_polygon(data = usa, aes(x = long, y = lat, alpha = 0.2, group = group), color = "white") + 
  geom_point(data = state_count, aes(x = long, y = lat, size = Cases, color = Cases)) +
  coord_fixed(1.3) +
  guides(fill=FALSE)



#states <- unique(usa$region)
#remove <- c('kansas','kentucky','iowa','montana','vermont','nebraska','rhode island')
#states <- states[!states %in% remove]
#state_count$name <- states
#write.csv(state_count,'states.csv')
#state_count <-read.csv('states.csv')

#state_count <- read.csv('states.csv')
#state_count$lat <- rep(0,nrow(state_count))
#state_count$long <- rep(0,nrow(state_count))
#for (i in 1:nrow(state_count)) {
#  for (j in 1:nrow(usa)) {
#    if (usa[j,'region'] == state_count[i,'name']) {
#      state_count[i,'lat'] = usa[j,'lat']
#      state_count[i,'long'] = usa[j,'long']
#    }
#  }
#}