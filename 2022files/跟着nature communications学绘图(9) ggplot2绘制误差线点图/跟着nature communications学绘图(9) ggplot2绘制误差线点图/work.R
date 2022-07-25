library(tidyverse)
library(ggprism)
library(ggsci)

df <- read_tsv("F1-b.txt") %>% pivot_longer(-c(type,time)) %>% 
  select(-name) %>% 
  group_by(type,time) %>% 
  summarise(value_mean=mean(value),sd=sd(value),se=sd(value)/sqrt(n())) %>% 
  arrange(time) %>% 
  filter(type !="w/o")

df$time <- factor(df$time,levels = c("10min","1h","6h","10h","24h","48h"))
df$type <- factor(df$type,levels=read_tsv("F1-b.txt") %>% pivot_longer(-c(type,time)) %>%
                    select(type) %>% distinct() %>% pull())

df %>% ggplot(aes(time,value_mean,fill=type,group=type,ymin=value_mean-se,ymax=value_mean+se))+
  geom_errorbar(width=0.1)+
  geom_line(color="black")+
  geom_point(key_glyph="polygon",aes(color=type))+
  geom_point(pch=21,size=5,show.legend = F)+
  scale_fill_npg()+
  scale_color_npg()+
  scale_x_discrete(guide = "prism_bracket")+
  scale_y_continuous(limits = c(0,300),minor_breaks = seq(0,300,50), guide = "prism_offset_minor")+
  annotate("text",x = 3,y=16,label="***",size=7,color="#35627F")+
  labs(x=NULL,y=NULL)+
  theme_prism(base_line_size =0.5)+
  theme(plot.margin=unit(c(0.1,0.1,0.1,0.1),units=,"cm"),
        axis.line = element_line(color = "black",size = 0.4),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.2,color = "#e5e5e5"),
        axis.text.y = element_text(color="black",size=10),
        axis.text.x = element_text(margin = margin(t = -5),color="black",size=10),
        legend.key=element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(color="black",size=8),
        legend.spacing.x=unit(0.1,'cm'),
        legend.spacing.y=unit(1,'cm'),
        legend.key.width=unit(0.7,'cm'),
        legend.key.height=unit(0.4,'cm'),
        legend.background=element_blank(),
        legend.box.margin = margin(0,0,0,0))







