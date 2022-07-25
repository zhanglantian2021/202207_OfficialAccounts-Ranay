library(tidyverse)
library(rstatix)
library(ggpubr)


stat.test <- iris %>% pivot_longer(-Species) %>%
  filter(Species !="versicolor") %>% 
  mutate(group=str_sub(name,start = 1,end = 5)) %>% 
  group_by(group,name) %>% 
  t_test(value ~ Species) %>%
  adjust_pvalue() %>% add_significance("p.adj") %>% 
  add_xy_position(x="name",scales="free",fun = "max") %>% 
  select(-3,-6,-7,-8,-9,-10) %>% 
  mutate(across("xmin",str_replace,"2.8","0.8"),
         across("xmin",str_replace,"3.8","1.8"),
         across("xmax",str_replace,"3.2","1.2"),
         across("xmax",str_replace,"4.2","2.2")) %>% 
  mutate(xmin=as.numeric(xmin),xmax=as.numeric(xmax))

iris %>% pivot_longer(-Species) %>%
  filter(Species !="versicolor") %>% 
  mutate(group=str_sub(name,start = 1, end = 5),
         group=as.factor(group)) %>% 
  ggplot(.,aes(x =name , y = value)) +
  stat_summary(geom = "bar",position = "dodge",aes(fill=Species)) +
  stat_summary(geom = "errorbar",fun.data = "mean_sdl",
               fun.args = list(mult = 1),
               aes(fill=Species),
               position=position_dodge(0.9),width=0.2,color="black") +
  facet_wrap(.~group,scale="free_x",nrow = 1)+
  stat_pvalue_manual(stat.test,label = "p.adj.signif",label.size=6,hide.ns = T,
                     tip.length = 0.01)+
  facet_wrap(.~group,scale="free_x",nrow = 1)+
  labs(x=NULL,y=NULL)+
  scale_fill_manual(values=c("#BA7A70","#829BAB"))+
  scale_y_continuous(limits = c(0, 9), expand = c(0, 0))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(color="black",size=12,margin = margin(r=3)),
        axis.ticks.x=element_blank(),
        axis.text.y=element_text(color="black",size = 10,margin = margin(r =2)),
        axis.text.x=element_text(color="black"),
        panel.background = element_rect(fill = NA,color = NA),
        panel.grid.minor= element_line(size=0.2,color="#e5e5e5"),
        panel.grid.major = element_line(size=0.2,color="#e5e5e5"),
        panel.border = element_rect(fill=NA,color="black",size=0.3,linetype="solid"),
        legend.key=element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(color="black",size=8),
        legend.spacing.x=unit(0.1,'cm'),
        legend.key.width=unit(0.5,'cm'),
        legend.key.height=unit(0.5,'cm'),
        legend.position = c(1,1),legend.justification=c(1,1),
        legend.background=element_blank(),
        legend.box.margin = margin(0,0,0,0),
        strip.text = element_text(color="black",size=10),
        panel.spacing.x=unit(0.3,"cm"))





