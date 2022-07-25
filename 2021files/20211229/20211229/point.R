library(tidyverse)
library(ggsci)


df <- read_csv("data.csv")

p <- read_tsv("group.xls") %>% left_join(.,df,by="sample") %>% 
  unite(.,col="group",group1:group2,sep="_",remove = T,na.rm = F) %>% 
  pivot_longer(Shannon.Wiener:pielou) %>% select(-sample)

p %>% ggplot(aes(x = group, y =value,fill =group,group=1)) +
  stat_summary(fun.data="mean_cl_normal",geom="errorbar",width=0.2) +
  stat_summary(fun = "mean",geom = "point",size=5,pch=21)+
  stat_summary(fun = "mean", geom = "line",size=0.5)+
  annotate("rect",xmin="Control_A",xmax="Control_B",ymin=-Inf,ymax=Inf,alpha=0.3,fill="grey")+

  facet_wrap(.~name,scales = "free",labeller = label_wrap_gen(),nrow = 2)+
  scale_fill_npg()+
  theme_test()+
  theme(panel.spacing.x = unit(0.2,"cm"),
        panel.spacing.y = unit(0.1, "cm"),
        axis.title = element_blank(),
        strip.text.x = element_text(size=9,color="black"),
        axis.text = element_text(color="black"),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.text = element_text(color="black",size=9),
        legend.title=element_blank(),
        legend.spacing.x=unit(0.1,'cm'),
        legend.key=element_blank(),
        legend.key.width=unit(0.4,'cm'),
        legend.key.height=unit(0.4,'cm'),
        legend.position = "top",
        plot.margin=unit(c(0.3,0.3,0.3,0.3),units=,"cm"))+
  guides(fill = guide_legend(direction = "horizontal"))+
  guides(fill=guide_legend(nrow=1, byrow=TRUE))