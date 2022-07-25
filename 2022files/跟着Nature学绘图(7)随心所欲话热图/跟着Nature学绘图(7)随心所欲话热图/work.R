library(tidyverse)
# install.packages("ggdendro")
library(ggdendro)
library(aplot)


df <- read_tsv("data.xls") %>% column_to_rownames(var="Pathway")
df2 <- df %>% rownames_to_column(var="Pathway") %>% pivot_longer(-Pathway)
hrdata <- hclust(dist(df)) %>% dendro_data(.,type = "rectangle")
hcdata <- hclust(dist(t(df))) %>% dendro_data(.,type = "rectangle")

df2$Pathway <- factor(df2$Pathway,levels = hrdata$labels %>% select(label) %>% pull())
df2$name <- factor(df2$name,levels = hcdata$labels %>% select(label) %>% pull())

heatmap <- df2 %>% ggplot(aes(name,Pathway,fill=value))+
  geom_tile()+
  labs(x=NULL,y=NULL)+
  scale_y_discrete(expand=c(0,0),position="left")+
  scale_x_discrete(expand=c(0,0))+
  scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(11,"RdBu")))+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size=8,color="black"))+
  guides(fill=guide_colorbar(direction = "vertical",reverse = F,barwidth = unit(.5, "cm"),
                             barheight = unit(10,"cm"))) 

group1 <- read_tsv("group.xls") %>% mutate(type="A") %>% 
  ggplot(aes(Sample_id,type,fill=`Subtype-1`))+
  geom_tile()+
  scale_fill_manual(values=c("#3B9AB2", "#78B7C5"))+
  theme_void()+
  theme(legend.title = element_blank(),
        legend.spacing.x=unit(0.1,'cm'),
        legend.key.width=unit(0.5,'cm'),
        legend.key.height=unit(0.5,'cm'),
        legend.background=element_blank(),
        legend.text = element_text(size=8,color="black"))
  
heatmap %>% insert_top(group1,height = 0.04)
