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
library(cowplot)

shp1 <- sf::read_sf("china.json")

p1 <- ggplot()+
  geom_sf(data=shp1,aes(fill = NULL))+
  annotation_scale(location = "br") + # 设置距离刻度尺
  annotation_north_arrow(location="tl",style = north_arrow_nautical(
                           fill = c("grey40","white"),line_col = "grey20"))+  # 添加指北针
  scale_fill_gradientn(colours=colors,na.value="transparent")+ 
  labs(x=NULL,y=NULL)+
  geom_sf(data=shp1,fill="NA",size=0.8,color="black")+ # 添加地图边界
  geom_point(aes(x =118.3375,y =29.747),color ="black",size =3,show.legend = F)+
  annotate("text",label = "huangshan",x =117.5, y =30.4,size =3, colour = "black")+
  xlim(112,150)+ylim(28,50)+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank())


lat=29.7147
long=118.3375
square_km=100

dem <- mapbox_dem(lat,long,square_km,
                  api="pk.eyJ1IjoiYmVueXNmIiwiYSI6ImNrczBtdWE0ajBwNjcydnBqMjRyZDdsOXkifQ.sUcMdooE7b9uQqzfrnWdSQ")

shp <- sf::read_sf("huangshan.json") 
hs <- raster::mask(dem,shp) %>% # 将地图与DEM数据结合
  crop(.,extent(shp))

df_hs <- as.data.frame(as(hs,"Raster"),xy=T)

colors <- c("#33A02C","#B2DF8A","#FDBF6F","#1F78B4","#999999",
            "#E31A1C","#E6E6E6","#A6CEE3")

shp2 <- sf::read_sf("huangshan.json")

p2 <- ggplot()+
  geom_sf(data=shp2,aes(fill = NULL))+
  scale_fill_gradientn(colours=colors,na.value="transparent")+ 
  labs(x=NULL,y=NULL)+
  geom_sf(data=shp2,fill="NA",size=0.8,color="black")+ 
  geom_tile(data=df_hs,aes(x=x,y=y,fill=layer),show.legend = F)+
  theme_void()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background = element_blank(),
        axis.text=element_blank(),
        legend.title = element_blank(),
        legend.position = "non")

ggdraw(p1) + draw_plot(p2,x=0.25,y=0.01,scale=.5)+
  geom_segment(aes(x =0.235, y =0.19, xend =0.75, yend =0.36))+
  geom_segment(aes(x =0.235, y =0.19, xend =0.53, yend =0.52))
  

  
