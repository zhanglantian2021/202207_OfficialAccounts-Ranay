

library(tidygeocoder)
library(ggrepel) 
library(ggtext)
library(tidyverse) 

chocolate <- readr::read_csv('chocolate.txt')

unique <- as.data.frame(table(distinct(chocolate %>% 
                                         select(c("company_location","country_of_bean_origin")))$country_of_bean_origin))


df <- distinct(chocolate %>% select(c("company_location","country_of_bean_origin")) %>% 
                 filter(country_of_bean_origin=="Madagascar")) %>% filter(company_location!=country_of_bean_origin)

# 使用geocode函数对位置进行地理编码（查找纬度和经度)

locs <- df %>% geocode(company_location,long=x.end,lat=y.end)
locs1 <- df %>% geocode(country_of_bean_origin,long=x,lat=y)
locs1 <- left_join(locs,locs1)

worldmap <- borders("world", colour="#00A08A", fill="#78B7C5")

ggplot() + 
  worldmap +
  geom_curve(data=locs1 %>% filter(x>x.end), aes(x=x,y = y, xend = x.end, yend = y.end),color="gray", curvature = -0.3) +
  geom_curve(data=locs1 %>% filter(x<x.end), aes(x=x,y = y, xend = x.end, yend = y.end),color="gray", curvature = 0.3) +
  ggrepel::geom_text_repel(data=locs1,aes(x=x.end,y=y.end,label=company_location),size=3)+
  geom_point(data=locs1,aes(x=x.end,y=y.end),color="brown",alpha=0.5)+
  geom_point(aes(x=46.44164,y=-18.92496))+
  theme_void()+
  theme(legend.position = "none",
        plot.background = element_rect(fill = 'white'))




