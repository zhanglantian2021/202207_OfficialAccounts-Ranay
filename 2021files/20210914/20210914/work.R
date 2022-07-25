package.list=c("tidyverse","ggspatial","ggsci",
               "scatterpie","ggrepel","tibble","ggpp","ComplexHeatmap")

for (package in package.list) {
  if (!require(package,character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

world_map <- map_data("world") %>% dplyr::rename(country=region) %>%
  mutate(country=str_replace(country,"Taiwan","China")) %>% 
  filter(country != "Antarctica") 

map <- c("Australia","China","Denmark","Finland","South Africa",
         "France","Germany","Russia","Egypt",
         "Hungary","Japan","Brazil","UK","USA","Canada") %>% 
  as.data.frame() %>% dplyr::rename(country=".") %>% 
  mutate(country=str_replace(country,"Korea","South Korea")) %>% 
  mutate(cou=country)

city <- read.delim("pie.xls",row.names = 1) %>% 
  t() %>% as.data.frame() %>% rownames_to_column(var="city") %>%
  left_join(.,read.delim("map.xls"),by="city")

world_map %>% left_join(.,map,by="country") %>% 
  ggplot()+
  geom_polygon(aes(x = long, y = lat,group = group,fill= cou),
               color = "black",size = 0.2,show.legend = F)+
  geom_scatterpie(data=city,aes(x=longitude,y=latitude,r=3),
                  cols=c("B","C","D"),show.legend=F)+
  geom_text_repel(data=city,aes(x=longitude,y=latitude,label=city),
                  min.segment.length=0,seed = 42,box.padding=1.2,
                  size=4,show.legend = F)+
  scale_fill_simpsons(na.value="gray70")+
  theme_void()+
  theme(plot.background = element_rect(fill="#5BBCD6",
                                       color="transparent",size = 1))+
  annotation_north_arrow(location = "bl",pad_x = unit(0.2,"in"),  
                         pad_y = unit(6,"in"),
                         style = north_arrow_nautical(fill = c("grey40","white"),
                                                      line_col = "grey20"))
lgd = Legend(labels = c("B","C","D"),
             legend_gp = gpar(fill=c("#FED439FF","#709AE1FF","#197EC0FF")),
             grid_width = unit(7,"mm"),grid_height=unit(7,"mm"))

draw(lgd,x = unit(0.99,"npc"),y = unit(0.6,"npc"),just = c("right","top"))





