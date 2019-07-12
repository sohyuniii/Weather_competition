setwd("C:/Users/user/Desktop/I/ewha2/Weather_competition/Data")
library(ggplot2)
library(dplyr)
library(readr)
library(ggmap)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)
library(leaflet)

map_df <- read_csv("gangnam_gps.csv")
cluster3 <- read_csv('cluster3_car_gn.csv')
cluster3 <- cluster3[,c(2,16)]
cluster4 <- read_csv('cluster4_car_gn.csv')
cluster4 <- cluster4[,c(2,16)]

table(cluster3$cluster)
table(cluster4$cluster)

map_df3 <- merge(cluster3, map_df, by='link_id')
map_df4 <- merge(cluster4, map_df, by='link_id')

##### ggmap ##### 
center <- c(mean(map_df3$start_lon),mean(map_df3$start_lat))
map <- get_map(center, zoom = 13, maptype ='roadmap')
ggmap(map) + geom_point(data=map_df3, aes(x=start_lon, y=start_lat, colour=cluster)) 

##### leaflet ##### 
pal <- colorFactor(c("navy", "red",'green'), domain = c(0,1,2))
leaflet(data = map_df3) %>% addTiles() %>%
  addCircleMarkers(lat = ~start_lat,lng = ~start_lon,
                   popup=paste(map_df3$link_id,'<br>',map_df3$start_point),
                   color = ~pal(cluster),
                   radius=3)
