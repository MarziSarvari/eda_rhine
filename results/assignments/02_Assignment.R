library(data.table)
library(ggplot2)
library(tidyverse)

### 1 Tidy Format Assignment 
runoff_stations_raw <- readRDS('./data/runoff_stations.rds')
runoff_stations <- runoff_stations_raw[,c(1,7,8)]
runoff_stations_tidy <- melt(runoff_stations, id.vars = "sname")
runoff_stations_tidy


### 2 geom_point plot 
ggplot(data = runoff_stations) +
  geom_point(aes(x = area, y = altitude ))



### 3


ggplot(data = runoff_stations, aes(x = area, y = altitude)) +
  geom_point() +
  geom_text(label = runoff_stations$sname) +
  theme_bw()

ggplot(data = runoff_stations_raw, aes(x = lon, y = lat, col = altitude)) +
  geom_point() +
  geom_text(label = runoff_stations_raw$sname) +
  scale_color_gradient(low = 'dark green', high = 'brown') +
  theme_bw()


### 4

ggplot(data=runoff_stations_raw, aes(x=sname, y=size)) +
  geom_bar(stat="identity", fill="brown")

