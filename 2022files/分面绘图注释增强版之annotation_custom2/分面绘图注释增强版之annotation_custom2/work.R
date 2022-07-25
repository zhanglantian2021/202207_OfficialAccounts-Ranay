library(tidyverse)
library(ggh4x)
library(ggsignif)
library(ggsci)
library(grid)

annotation_custom2 <- function (grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, data)
{
  layer(data = data, stat = StatIdentity, position = PositionIdentity, 
        geom = ggplot2:::GeomCustomAnn,
        inherit.aes = TRUE, params = list(grob = grob, 
                                          xmin = xmin, xmax = xmax, 
                                          ymin = ymin, ymax = ymax))
}


grob <- grobTree(textGrob("Scatter plot", x=0.1,  y=0.95, hjust=0,
                          gp=gpar(col="red", fontsize=13, fontface="italic")))


data.segm<-data.frame(x=5,xend=6,y=3,yend=3,name="Sepal.Width")


plot <- ggplot()+ geom_segment(data=data.segm,color="red",
             aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE)+theme_void()


mtcars%>% ggplot(aes(mpg,disp))+
  geom_point()+facet_grid(vs~am)+
  annotation_custom2(grob,data = mtcars %>% filter(vs == 1,am==1))



p1 <- iris %>% pivot_longer(-Species) %>% 
  mutate(type=name) %>% 
  separate(type,into=c("type","type2",sep=".")) %>% 
  select(-.,-type2) %>% 
  ggplot(aes(Species,value,fill=Species))+
  stat_boxplot(geom="errorbar",
               position=position_dodge(width=0.8),width=0.2)+
  geom_boxplot(position=position_dodge(width =0.8))

pal <- c("#E64B35FF","#4DBBD5FF","#00A087FF","#F39B7FFF","#3C5488FF","#91D1C2FF")


data.segm<-data.frame(x=0.5,xend=2,y=1,yend=1)


plot2 <- ggplot()+ geom_segment(data=data.segm,color="black",
                               aes(x=x,y=y,yend=yend,xend=xend),inherit.aes=FALSE)+
  theme_void()


grob2 <- grobTree(textGrob("***", x=1.8,  y=2.1,hjust=0,
                          gp=gpar(col="red", fontsize=18)))


 p1 + facet_nested(.~type+name,drop=T,scale="free",space="free",switch="y",
                  strip =strip_nested(background_x =elem_list_rect(fill =pal),by_layer_x = F))+
  theme(panel.spacing.x = unit(0, "cm"))+
  annotation_custom2(grob=ggplotGrob(plot2),data =iris %>% pivot_longer(-Species) %>% 
                       mutate(type=name) %>% 
                       separate(type,into=c("type","type2",sep=".")) %>% 
                       select(-.,-type2) %>% filter(name=="Petal.Length"),
                     ymin =2, ymax=2, xmin=0.8, xmax=Inf)+
   annotation_custom2(grob=ggplotGrob(plot2),data =iris %>% pivot_longer(-Species) %>% 
                        mutate(type=name) %>% 
                        separate(type,into=c("type","type2",sep=".")) %>% 
                        select(-.,-type2) %>% filter(name=="Petal.Width"),
                      ymin =2, ymax=2, xmin=0, xmax=1.1)+
  annotation_custom2(grob=grob2,data =iris %>% pivot_longer(-Species) %>% 
                       mutate(type=name) %>% 
                       separate(type,into=c("type","type2",sep=".")) %>% 
                       select(-.,-type2) %>% filter(name=="Petal.Length"),
                     ymin =2.1, ymax=2.1, xmin=1.8, xmax=2)+
   annotation_custom2(grob=ggplotGrob(plot2),data =iris %>% pivot_longer(-Species) %>% 
                        mutate(type=name) %>% 
                        separate(type,into=c("type","type2",sep=".")) %>% 
                        select(-.,-type2) %>% filter(name=="Petal.Length"),
                      ymin =8, ymax=8, xmin=Inf,xmax=Inf)+
   annotation_custom2(grob=ggplotGrob(plot2),data =iris %>% pivot_longer(-Species) %>% 
                        mutate(type=name) %>% 
                        separate(type,into=c("type","type2",sep=".")) %>% 
                        select(-.,-type2) %>% filter(name=="Petal.Width"),
                      ymin =8, ymax=8, xmin=-0.8,xmax=Inf)+
   annotation_custom2(grob=ggplotGrob(plot2),data =iris %>% pivot_longer(-Species) %>% 
                        mutate(type=name) %>% 
                        separate(type,into=c("type","type2",sep=".")) %>% 
                        select(-.,-type2) %>% filter(name=="Sepal.Length"),
                      ymin =8, ymax=8, xmin=-0.8,xmax=3.3)+
   annotation_custom2(grob=grob2,data =iris %>% pivot_longer(-Species) %>% 
                        mutate(type=name) %>% 
                        separate(type,into=c("type","type2",sep=".")) %>% 
                        select(-.,-type2) %>% filter(name=="Petal.Width"),
                      ymin =8.05, ymax=8.05, xmin=1.8, xmax=2)+
   
  labs(x=NULL,y=NULL)+scale_fill_brewer()+
  theme_minimal()+
  theme(axis.text.x = element_blank(),
        panel.spacing.x = unit(0, "cm"),
        legend.position = "npn")


  


  


