library(tidyverse)
library(showtext)
library(scales)
library(ggprism)

sports <- read_tsv("sports.xls")

font_add_google(name = "Black Ops One", family = "opsone")
font_add_google(name = "Dosis", family = "dosis")
showtext_auto()

plot_data <- sports %>% 
  select(exp_men, exp_women, sports) %>%
  drop_na() %>% 
  filter(exp_men!= 0,exp_women!= 0) %>% 
  group_by(sports) %>% 
  summarise(mean_exp_men=mean(exp_men), 
            mean_exp_women=mean(exp_women)) %>% 
  mutate(mean_exp_diff=mean_exp_men - mean_exp_women) %>% 
  select(sports,mean_exp_diff) %>% 
  mutate(sports=fct_reorder(sports, mean_exp_diff), 
         less=as.factor(mean_exp_diff<0))

ggplot(data=plot_data, 
       mapping=aes(x=mean_exp_diff,y=sports, colour = less)) +
  geom_point(size = 3) +
  geom_segment(aes(yend=sports,xend=0),size=1) +
  annotate("segment",y=1,yend=17,x=150000,xend=150000,colour="#4b0082",
           arrow = arrow(ends="both",angle=90,length=unit(.2,"cm"))) +
  annotate("segment",y=18,yend=31,x=-150000,xend=-150000,colour="#008080",
           arrow=arrow(ends="both",angle=90,length=unit(.2,"cm"))) +
  annotate("text",x=275000,y=9,label="Higher female expenditure",size=8,
           colour="#4b0082",family="dosis") +
  annotate("text",x=-275000,y=24.5,label="Higher male expenditure",size=8,
           colour="#008080",family="dosis") +
  scale_x_continuous(labels=unit_format(unit="K",scale=1e-3,sep=""), 
                     limits=c(-600000,600000),expand=c(0,0),guide="prism_offset_minor") +
  scale_colour_manual("",values=c("#008080","#4b0082")) +
  coord_flip() +
  labs(x=NULL,y=NULL)+
  theme_prism(base_line_size=0.5)+
  theme(plot.margin=unit(c(0.8,0.8,0.5,0.8),"cm"), 
        axis.line=element_line(color="black"),
        axis.text.x=element_text(angle = 90,hjust=1,color="black"),
        axis.text.y = element_text(color="black"),
        panel.grid.major =element_line(size=0.2,color="#e5e5e5"),
        panel.grid.minor=element_blank(),
        panel.spacing=unit(0,"lines"),
        legend.position="none")
