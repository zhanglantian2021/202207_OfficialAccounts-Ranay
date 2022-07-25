library(geoviz)
library(tidyverse)
library(sf)
library(terra)
library(rasterVis)
library(ggspatial)
library(rgdal)
library(rnaturalearth)
library(rnaturalearthdata)
library(raster)

lat=35.3940
long=109.1880
square_km=400

# mapbox_dem 获取DEM数据

dem <- mapbox_dem(lat,long,square_km,
                  api="pk.eyJ1IjoiYmVueXNmIiwiYSI6ImNrczBtdWE0ajBwNjcydnBqMjRyZDdsOXkifQ.sUcMdooE7b9uQqzfrnWdSQ")

shp <- sf::read_sf("shanxi.json") # 读入陕西省地图

shanxi<- raster::mask(dem,shp) %>% # 将地图与DEM数据结合
  crop(.,extent(shp))

df_shanxi <- as.data.frame(as(shanxi,"Raster"),xy=T) #格式转换

# 下载世界河流信息
rivers <- ne_download(scale = 10, type = 'rivers_lake_centerlines',category = 'physical')

# 后续要将河流信息添加到ggplot2图中，因此通过 st_as_sf将其转换为sf

rivers_cropped <- st_crop(st_as_sf(rivers), xmin = 107, xmax =111,
                          ymin = 32, ymax =39)

colors <- c("#33A02C","#B2DF8A","#FDBF6F","#1F78B4","#999999",
             "#E31A1C","#E6E6E6","#A6CEE3")

ggplot()+
  geom_sf(data=shp,aes(fill = NULL))+
  annotation_scale(location = "bl") +
  annotation_north_arrow(location="tl",
                         style = north_arrow_nautical(
                           fill = c("grey40","white"),
                           line_col = "grey20"))+
  geom_tile(data=df_shanxi,aes(x=x,y=y,fill=layer),show.legend = F)+
  geom_sf(data = rivers_cropped,col='blue',size=1)+
  scale_fill_gradientn(colours=colors,na.value="transparent")+
  labs(x=NULL,y=NULL)+
  geom_sf(data=shp,fill="NA",size=1,color="black")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank())
