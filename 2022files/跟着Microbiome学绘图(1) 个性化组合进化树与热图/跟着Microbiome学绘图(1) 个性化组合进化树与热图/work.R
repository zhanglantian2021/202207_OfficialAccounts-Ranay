library(tidyverse)
library(ggtree)
library(ggdendro)
library(patchwork)

df <- read_csv("Figure5-metabolicfunctions_logtpm.csv") %>% dplyr::rename(tax=`...1`) %>% 
  column_to_rownames(var="tax")

tree <- hclust(dist(df)) %>% ggtree() %>% 
  collapse(174, 'min', fill="#E41A1C")  %>% 
  collapse(173, 'max', fill="#1E90FF") %>% 
  collapse(146, 'mix', fill="#FF8C00") %>% 
  collapse(141, 'mixed', fill="#4C005C") %>% 
  collapse(155, 'min', fill="#003380") %>% 
  collapse(154, 'min', fill="#FF8C00") %>%
  collapse(155, 'min', fill="#FF8C00") %>%
  collapse(152, 'mixed', fill="#740AFF") %>%
  collapse(151, 'min', fill="#FF8C00") %>% 
  collapse(150, 'min', fill="#8F7C00") %>% 
  collapse(143, 'min', fill="#E41A1C") %>% 
  collapse(144, 'min', fill="#94FFB5") %>% 
  collapse(159, 'min', fill="#0075DC") %>% 
  collapse(158, 'min', fill="#00FFFF") %>% 
  collapse(156, 'min', fill="#E41A1C") %>% 
  collapse(148, 'min', fill="#8F7C00") %>% 
  collapse(140, 'min', fill="#E41A1C")

hrdata <- hclust(dist(df)) %>% dendro_data(.,type="rectangle")
hcdata <- hclust(dist(t(df))) %>% dendro_data(.,type="rectangle")

df2 <- read_csv("Figure5-metabolicfunctions_logtpm.csv") %>% dplyr::rename(tax=`...1`) %>% 
  pivot_longer(-tax)

df2$tax <- factor(df2$tax,levels = hrdata$labels %>% select(label) %>% pull())
df2$name <- factor(df2$name,levels = hcdata$labels %>% select(label) %>% pull())

heatmap <- df2 %>% ggplot(aes(name,tax,fill=value))+
  geom_tile()+
  labs(x=NULL,y=NULL)+
  scale_y_discrete(expand=c(0,0),position="left")+
  scale_x_discrete(expand=c(0,0))+
  scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(11,"RdBu")))+
  theme(axis.text.x=element_text(color="black",angle=90,vjust=0.5,size=8,hjust=1),
        axis.text.y=element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size=8,color="black"))+
  guides(fill=guide_colorbar(direction = "vertical",reverse = F,barwidth = unit(.6, "cm"),
                             barheight = unit(18.5,"cm"))) 

tree + heatmap+ plot_layout(widths = c(.3,1))



