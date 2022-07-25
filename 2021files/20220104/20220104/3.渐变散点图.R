library(tidyverse)
library(ggnewscale)
library(viridis)

df <- read_csv("crime_immig.csv")

df %>% ggplot() + 
  geom_point(aes(totalpop,burg,fill=burg),color="black",shape=21, size=5)+
  geom_smooth(aes(totalpop,burg,colour=burg),size=1)+
  scale_fill_distiller(name="Point",palette="YlOrRd")+
  theme_bw()+
  theme(panel.grid=element_blank(),
        legend.position = "none")


df %>% ggplot() + 
  geom_point(aes(totalpop,burg,fill=burg),color="black",shape=21,size=5)+
  geom_smooth(aes(totalpop,burg,color=..y..),size=1,fill="lightblue")+
  scale_fill_distiller(name="Point",palette = "Spectral")+
  scale_color_distiller(name="Point",palette = "Spectral")+
  theme_bw()+
  theme(panel.grid=element_blank(),legend.position = "none")

B<- read_csv("ABC.CSV")

ggplot(B,aes(x = Year))+
  geom_point(aes(y = burg,colour=burg),size=3)+ 
  geom_line(aes(y =burg,colour=burg),size=1) +
  scale_color_gradient(low = "blue", high = "red")+
  theme_bw()+
  theme(panel.grid=element_blank(),
        legend.position = "none")


ggplot(B, aes(x = Year))+
  geom_point(aes(y = burg,colour=burg),size=3)+ 
  geom_line(aes(y =burg,colour=burg),size=1) +
  scale_color_gradient(low = "red", high = "blue")+
  new_scale_color() + # 此处将color同时映射给了两种不同的尺度
  geom_point(aes(y =(rob)/2,colour=rob),size=3)+ 
  geom_line(aes(y =(rob)/2,colour=rob),size=1) +
  scale_color_viridis()+
  scale_y_continuous(name = 'burg',
                     sec.axis = sec_axis(~.*2,name = 'rob'))+
  xlab("Year")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.background = element_blank(),
        legend.text = element_text(size=10),
        legend.position = c(0.02,0.98),
        legend.direction = "vertical", 
        legend.box = "horizontal",
        legend.justification = c(0,1),
        axis.title =  element_text(size=12,face = "bold"),
        axis.ticks.length = unit(-0.15, "cm"),
        axis.text.x = element_text(margin = margin(t = 7),vjust = 0.4,size = 10),
        axis.text.y.left = element_text(margin = margin(r = 7),size = 10),
        axis.text.y.right =element_text(margin = margin(l = 7,r = 7),size=10))+
  scale_x_continuous(breaks = seq(1980,2021,5))



亮丽彩虹色scale_color_gradientn(colours =rainbow(10))
红蓝渐变scale_color_gradient(low = "blue", high = "red")
红白蓝渐变scale_color_gradient2(low = "red", mid = "white", high = "blue")
复古彩虹色scale_color_distiller(palette = "Spectral")
红黄渐变色scale_color_distiller(palette="YlOrRd")
黄绿渐变色scale_color_viridis_c()
紫黄渐变色scale_color_viridis(option = "plasma")


