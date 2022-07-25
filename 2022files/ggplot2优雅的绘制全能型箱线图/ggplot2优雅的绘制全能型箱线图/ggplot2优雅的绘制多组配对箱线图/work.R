library(tidyverse)
library(gapminder)
library(ggsci)
library(ggprism)
library(rstatix)
library(ggpubr)
library(ggpmisc)

df <- gapminder %>%
  filter(year %in% c(1957,2002,2007),continent !="Oceania") %>%
  select(country,year,lifeExp,continent)%>%
  mutate(paired = rep(1:(n()/3),each=3),year=factor(year))

df_p_val1 <- df %>% group_by(continent) %>%
  wilcox_test(lifeExp  ~ year) %>%
  adjust_pvalue(p.col = "p", method = "bonferroni") %>%
  add_significance(p.col = "p.adj") %>% 
  add_xy_position(x = "year", dodge = 0.8) 

df %>%
  ggplot(aes(year,lifeExp)) +
  stat_boxplot(geom="errorbar",position=position_dodge(width=0.2),width=0.1)+
  geom_boxplot(position=position_dodge(width =0.2),width=0.4)+
#  geom_line(aes(group=paired),position = position_dodge(0.2),color="grey80") +
  geom_point(aes(fill=year,group=paired,size=lifeExp,alpha=lifeExp),pch=21,
             position = position_dodge(0.2))+
  stat_pvalue_manual(df_p_val1,label = "p.adj.signif",label.size=5,hide.ns = F)+
  scale_size_continuous(range=c(1,3))+
  geom_smooth(method = "lm", formula = NULL,size=1,se=T,color="black",linetype="dashed",aes(group=1))+
  stat_cor(label.y = 25,aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"),group=1),color="black",
           label.x.npc = "left")+
  stat_regline_equation(label.y = 19,aes(group=1),color="red")+
  facet_wrap(.~continent,nrow=1)+
  scale_fill_npg()+
  scale_x_discrete(guide = "prism_bracket")+
  scale_y_continuous(limits = c(0,95),minor_breaks = seq(0,95,5),guide = "prism_offset_minor")+
  labs(x=NULL,y=NULL)+
  theme_prism(base_line_size =0.5)+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),units=,"cm"),
        strip.text = element_text(size=12),
        axis.line = element_line(color = "black",size = 0.4),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.2,color = "#e5e5e5"),
        axis.text.y = element_text(color="black",size=10),
        axis.text.x = element_text(margin = margin(t = -5),color="black",size=10),
        legend.position = "none",
        panel.spacing = unit(0,"lines"))+
  coord_cartesian()
