library(tidyverse)
library(ggsignif)
library(ggsci)

iris %>% pivot_longer(-Species) %>% 
  mutate(name=as.factor(name)) %>% 
  ggplot(aes(name,value,fill=Species))+
  stat_boxplot(geom="errorbar",
               position=position_dodge(width=0.8),width=0.2)+
  geom_boxplot(position=position_dodge(width =0.8))


p <- iris %>% pivot_longer(-Species) %>% 
  mutate(name=as.factor(name)) %>% 
  ggplot(aes(Species,value,fill=Species))+
  stat_boxplot(geom="errorbar",
               position=position_dodge(width=0.8),width=0.2)+
  geom_boxplot(position=position_dodge(width =0.8))+
  geom_signif(comparisons = list(c("setosa", "versicolor"),
                                 c("virginica","versicolor"),
                                 c("setosa","virginica")),
              map_signif_level=T,vjust=0.5,color="black",
              textsize=5,test=wilcox.test,step_increase=0.1)+
  facet_wrap(.~name,nrow=1)+
  scale_fill_jco()+
  theme(legend.title=element_blank())+
  labs(x=NULL,y=NULL)+
  theme_classic()+
  theme(strip.background = element_rect(fill="grey80",color="black"),
        strip.text.x = element_text(size=10,color="black"),
        axis.text.y=element_text(size=10,color="black"),
        axis.ticks.x = element_blank(),
        axis.text.x=element_blank(),
        panel.spacing = unit(0,"lines"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        legend.text = element_text(color="black",size=10),
        legend.title=element_blank(),
        legend.spacing.x=unit(0.1,'cm'),
        legend.key=element_blank(),
        legend.key.width=unit(0.6,'cm'),
        legend.key.height=unit(0.6,'cm'),
        plot.margin=unit(c(0.3,0.3,0.3,0.3),units=,"cm"))

p

data.segm<-data.frame(x=0.5,y=5,xend=4,yend=5,name="Sepal.Width")

ann_text <- data.frame(Species="versicolor",value=5.5,lab = "Text",
                       name= factor("Sepal.Width",
                                    levels = c("Petal.Length","Petal.Width",
                                               "Sepal.Length","Sepal.Width")))
p + geom_text(data = ann_text,label = "Sepal.Width")+
  geom_segment(data=data.segm,color="red",
               aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE)
