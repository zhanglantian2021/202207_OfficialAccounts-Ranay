library(tidyverse)
library(ggbump)
library(ggsci)

df <- read_tsv("data.xls") %>% 
  select(1,2,3,4) %>% 
  mutate(year=as.character(year)) %>% as.data.frame() %>% 
  filter(continent %in% c("Europe","Americas"),year %in% c("1952","2007")) %>% 
  group_by(year,continent) %>% 
  mutate(percent=round(100*lifeExp/sum(lifeExp),digits=2)) %>% 
  top_n(3) %>% 
  arrange(year,continent) %>% 
  ungroup()

df %>% ggplot()+
  geom_bar(aes(fill = country, y =percent, x=year), position = "fill",
           stat = "identity", width = .5,key_glyph="dotplot")+
  geom_text(aes(y = percent, x =year, label = paste0(percent, "%")),
            size = 4, position = position_fill(vjust = 0.5),color="white") +
  geom_text(aes(x = year, y = -0.05, label = year),color="black",hjust = 1, nudge_y =-0.01, size = 4) +
  geom_text(aes(x = 1.5, y = 0, label =continent), hjust = 0.5,nudge_y = -0.4, size = 5, stat = "unique",color="black") +
  geom_sigmoid(aes(x = 2.25, xend = 1.5, y = -0.19, yend = -0.265),size = 0.2,direction = "y",color="black",smooth =8) +
  geom_sigmoid(aes(x = 0.75, xend = 1.5, y = -0.19, yend = -0.265),size = 0.2,direction = "y",color="black",smooth =8)+
  coord_flip(clip = "off", expand = FALSE) +
  scale_fill_futurama()+
  facet_wrap(vars(continent),ncol = 1) +
  theme_void() +
  theme(plot.margin = margin(30,30,30,50),
        strip.text = element_blank(),
        panel.spacing.y = unit(2,"lines"),
        legend.title = element_blank(),
        legend.key=element_blank(), 
        legend.text = element_text(color="black",size=10), 
        legend.spacing.x=unit(0.1,'cm'), 
        legend.key.width=unit(0.6,'cm'), 
        legend.key.height=unit(0.6,'cm'),
        legend.background=element_blank(), 
        legend.box.background=element_rect(colour = "black"),
        legend.box.margin = margin(0,2,0,0))
