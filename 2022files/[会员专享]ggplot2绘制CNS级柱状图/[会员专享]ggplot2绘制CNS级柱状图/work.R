library(tidyverse)
library(gapminder)
library(ggalt)
library(ggbreak)
library(MetBrewer)
library(ggpmisc)


p <- gapminder %>% filter(continent=="Africa",country=="Angola") %>% 
  select(year,lifeExp) %>%
  ggplot(.,aes(year,lifeExp))+
  geom_point()+
  geom_line()

p + geom_col()


gapminder %>% filter(continent=="Africa",country=="Angola") %>% 
  select(year,lifeExp) %>%
  ggplot(.,aes(year,lifeExp))+
  geom_col(aes(fill=lifeExp),width=3.5)+
  geom_xspline(spline_shape =-0.5,color="#D68E24",size=1)+
  geom_point(size=3,pch=19,color='#139E56')+
  geom_text(aes(label=year,y=29.6),color="white")+
  geom_smooth(method = "lm", formula = NULL, size=1,se=F,color="black",linetype="dashed")+
  scale_y_break(c(0,30),space=0)+
  scale_y_continuous(limits = c(0,43),expand = c(0,0),
                     breaks = c(30,32,34,36,38,40,42),
                     labels = c(30,32,34,36,38,40,42))+
  labs(x=NULL, y=NULL)+
  scale_fill_gradientn(colors=met.brewer("Troy"))+
  theme_test()+
  theme(axis.title =element_text(color="black",size=12),
        axis.text.x = element_blank(),
        axis.ticks.x =element_blank(),
        axis.text.y = element_text(size = 10,color="black"),
        legend.text = element_text(size=10),
        legend.position = "bottom",
        legend.title = element_blank())+
  stat_poly_eq(formula = y ~ x,aes(label = paste(..eq.label.., ..rr.label..,p.value.label,sep = "~~~")), 
               parse=TRUE,label.x.npc = "left",size=5,color="black")+
  guides(fill=guide_colorbar(direction="horizontal",reverse=F,barwidth=unit(18,"cm"),
                              barheight=unit(.5,"cm")))



