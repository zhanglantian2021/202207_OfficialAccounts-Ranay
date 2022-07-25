library(tidyverse)
library(grid)
library(RColorBrewer)
library(ggh4x)
library(scales)
library(aplot)

sports <- read_tsv("sports.xls")

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
         less=as.factor(mean_exp_diff<0)) %>% 
  arrange(less,mean_exp_diff) %>% 
  rownames_to_column(var="group")
  
plot_data$group <- factor(plot_data$group,levels =plot_data$group)
  
g <- rasterGrob(brewer.pal(n = 8,name = "RdBu")[5:8],width=unit(1,"npc"), height = unit(1,"npc"), 
                interpolate = TRUE)


h <- rasterGrob(blues9[1], width=unit(1,"npc"), height = unit(1,"npc"), interpolate = TRUE) 
f<- rasterGrob(brewer.pal(n = 8,name = "RdBu")[3:4], width=unit(1,"npc"), height = unit(1,"npc"), 
               interpolate = TRUE) 


p1 <- ggplot(data=plot_data,mapping=aes(x=mean_exp_diff,y=group,colour = less))+
  annotation_custom(g,xmin=-Inf,xmax=Inf,ymin=0,ymax=11)+
  annotation_custom(h,xmin=-Inf,xmax=Inf,ymin=11,ymax=22)+
  annotation_custom(f, xmin=-Inf,xmax=Inf,ymin=22,ymax=Inf)+
  geom_point(size=4) +
  geom_segment(aes(yend=group,xend=0),size=1)+
  scale_x_continuous(labels=unit_format(unit="K",scale=1e-3,sep=""), 
                     limits=c(-600000,600000),expand=c(0,0),
                     guide = "axis_truncated") +
  scale_colour_manual(values=c("#008080","#4b0082"))+
  labs(x=NULL,y=NULL)+
  theme_void()+
  theme(axis.line.x = element_line(),
        legend.position = "non")

p2 <- plot_data %>% select(1,2) %>% mutate(type="A",group=as.numeric(group)) %>% 
  mutate(type2=case_when(group <= 11 ~ "A",group > 11& group<=22 ~"B",
                         group > 22 ~"C")) %>%
  ggplot(aes(type,group))+
  annotation_custom(g,xmin=-Inf,xmax=Inf,ymin=0,ymax=11)+
  annotation_custom(h,xmin=-Inf,xmax=Inf,ymin=11,ymax=22)+
  annotation_custom(f, xmin=-Inf,xmax=Inf,ymin=22,ymax=Inf)+
  geom_text(aes(type,group,label=sports,color=type2),size=4,show.legend = F)+
  scale_colour_manual(values=c("#8C0C25","#008080","#4b0082"))+
  theme_void()

p1 %>% insert_left(p2,width = .4)


p1 <- ggplot(data=plot_data,mapping=aes(x=mean_exp_diff,y=group,colour = less))+
  geom_point(size=4) +
  geom_segment(aes(yend=group,xend=0),size=1)+
  scale_x_continuous(labels=unit_format(unit="K",scale=1e-3,sep=""), 
                     limits=c(-600000,600000),expand=c(0,0),
                     guide = "axis_truncated") +
  scale_colour_manual(values=c("#008080","#4b0082"))+
  labs(x=NULL,y=NULL)+
  theme_void()+
  theme(axis.line.x = element_line(),
        legend.position = "non")

p2 <- plot_data %>% select(1,2) %>% mutate(type="A",group=as.numeric(group)) %>% 
  mutate(type2=case_when(group <= 11 ~ "A",group > 11& group<=22 ~"B",
                         group > 22 ~"C")) %>%
  ggplot(aes(type,group))+
  geom_text(aes(type,group,label=sports,color=type2),size=4,show.legend = F)+
  scale_colour_manual(values=c("#8C0C25","#008080","#4b0082"))+
  theme_void()

p1 %>% insert_left(p2,width = .4)


reds <- colorRampPalette(brewer.pal(11,"RdBu"))(21)

grid.raster(scales::alpha(reds, 0.5), width = unit(1, "npc"), height = unit(1,"npc"),interpolate = TRUE)




