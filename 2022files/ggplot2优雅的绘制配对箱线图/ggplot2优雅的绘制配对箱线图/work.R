library(tidyverse)
library(ggsignif)
library(ggsci)
library(ggprism)

df <- read_tsv("data.xls") %>% 
  filter(year %in% c(1957,2007),continent !="Oceania") %>% 
  select(country,year,lifeExp,continent)%>%
  mutate(paired = rep(1:(n()/2),each=2),year=factor(year))

df %>%
  ggplot(aes(year,lifeExp)) +
  stat_boxplot(geom="errorbar",position=position_dodge(width=0.2),width=0.1)+
  geom_boxplot(position=position_dodge(width =0.2),width=0.4)+
  geom_line(aes(group=paired),position = position_dodge(0.2),color="grey80") +
  geom_point(aes(fill=year,group=paired,size=lifeExp,alpha=lifeExp),pch=21,
             position = position_dodge(0.2))+
  scale_size_continuous(range=c(1,3))+
  geom_signif(comparisons = list(c("1957","2007")),
              map_signif_level=T,vjust=0.5,color="black",
              textsize=5,test=wilcox.test,step_increase=0.1)+
  facet_wrap(.~continent,nrow=1)+
  scale_fill_npg()+
  scale_x_discrete(guide = "prism_bracket")+
  scale_y_continuous(limits = c(0,90),minor_breaks = seq(0,90,5),guide = "prism_offset_minor")+
  labs(x=NULL,y=NULL)+
  theme_prism(base_line_size =0.5)+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),units=,"cm"),
    axis.line = element_line(color = "black",size = 0.4),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(size = 0.2,color = "#e5e5e5"),
    axis.text.y = element_text(color="black",size=10),
    axis.text.x = element_text(margin = margin(t = -5),color="black",size=10),
    legend.position = "none",
    panel.spacing = unit(0,"lines"))+
  coord_cartesian()



