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
cluster3 <- read_csv('cluster_df.csv')
map_df3 <- merge(cluster3, map_df, by='link_id')
table(map_df3$cluster)


##### leaflet ##### 

map_df3 <- map_df3 %>% mutate(m_lat = (start_lat+end_lat)/2,
                              m_lon = (start_lon+end_lon)/2)

pal <- colorFactor(c("navy", "red",'green'), domain = c(0,1,2))
leaflet(data = map_df3) %>% addTiles() %>%
  addCircleMarkers(lat = ~m_lat,lng = ~m_lon,
                   popup=paste(map_df3$link_id,'<br>',map_df3$road_name),
                   color = ~pal(cluster),
                   radius=3)

leaflet(data = map_df3) %>% addTiles() %>%
  addCircleMarkers(lat = ~end_lat,lng = ~end_lon,
                   popup=paste(map_df3$link_id,'<br>',map_df3$road_name),
                   color = ~pal(cluster),
                   radius=3)

clus0 <- filter(map_df3,cluster==0)
clus1 <- filter(map_df3,cluster==1)
clus2 <- filter(map_df3,cluster==2)

leaflet(data = clus1) %>% addTiles() %>%
  addCircleMarkers(lat = ~end_lat,lng = ~end_lon,
                   popup=paste(map_df3$link_id,'<br>',map_df3$road_name),
                   radius=3) %>%
  addCircleMarkers(lat = ~start_lat,lng = ~start_lon,
                   popup=paste(map_df3$link_id,'<br>',map_df3$road_name),
                   radius=3,color='red')

##### ggmap ##### 

center <- c(mean(map_df3$start_lon),mean(map_df3$start_lat))
map <- get_map(center, zoom = 13, maptype ='roadmap')

ggmap(map) + geom_point(data=gangnam, aes(x=lon, y=lat, size=call, colour=call)) +
  scale_color_gradient(low='blue', high='red')



road_map <- function(df){
  ggmap(map) + 
    geom_segment(data = df,
                 aes(x = start_lon, 
                     y = start_lat,
                     xend = end_lon,
                     yend = end_lat),
                 color = "#008b8b",size=1.3) + 
    geom_point(data=df, aes(x=start_lon, y=start_lat),color = "#ff3f13",size=2) +
    geom_point(data=df, aes(x=end_lon, y=end_lat),color = "#ff3f13",size=2) +
    theme_nothing()
}

road_map(clus0)
road_map(clus1)
road_map(clus2)
road_map(map_df3)

