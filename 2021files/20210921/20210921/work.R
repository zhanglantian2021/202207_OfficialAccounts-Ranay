library(wesanderson)
library(magrittr)
library(tidyverse)
library(clusterProfiler)
load("data.RData")

pal <- wes_palette("Zissou1", 100, type = "continuous")

kegg <- read.delim("Kegg.xls") %>% 
  select(2,3,6) %>%
  separate(`GeneRatio`,into="GeneRatio",sep="/") %>% 
  arrange(desc(p.adjust))

kegg$Description <- factor(kegg$Description,levels=kegg$Description,ordered = T)

ggplot(kegg,aes(Description,GeneRatio,fill=p.adjust))+
  geom_col()+
  scale_fill_gradientn(colours=pal)+
  coord_flip()+
  scale_x_discrete(expand = c(0, 0)) +
  theme_test()+
  theme(axis.title = element_blank(),
        axis.text=element_text(color="black"),
        legend.key=element_blank(), 
        legend.title = element_text(color="black",size=10), 
        legend.text = element_text(color="black",size=8), 
        legend.spacing.x=unit(0.1,'cm'), 
        legend.key.width=unit(0.5,'cm'),
        legend.key.height=unit(0.5,'cm'), 
        legend.background=element_blank(), 
        legend.box.margin = margin(1,1,1,1),
        legend.position = c(0.99,0),legend.justification = c(0.99,0))
#-------------------------------------------------------------------------------
read.delim("Kegg.xls") %>% 
  select(2,3,6) %>%
  separate(`GeneRatio`,into=c("A","B"),sep="/") %>% 
  mutate(A=as.numeric(A),B=as.numeric(B)) %>% 
  mutate(count=A/B) %>% 
  ggplot(aes(count,fct_reorder(Description,count)))+
  geom_point(aes(size=A,color=p.adjust))+
  scale_color_gradientn(colours=pal)+
  guides(size=guide_legend(title="Count"))+
  theme_test()+
  theme(axis.title = element_blank(),
        axis.text=element_text(color="black"),
        legend.key=element_blank(), 
        legend.title = element_text(color="black",size=10), 
        legend.text = element_text(color="black",size=8), 
        legend.spacing.x=unit(0.1,'cm'), 
        legend.key.width=unit(0.5,'cm'),
        legend.key.height=unit(0.5,'cm'), 
        legend.background=element_blank(), 
        legend.box.margin = margin(1,1,1,1))
#-------------------------------------------------------------------
cnetplot(dd,
         foldChange = geneList, 
         showCategory = 3,
         node_label = "none", # category | gene | all | none
         circular = TRUE, 
         colorEdge = TRUE)+
  theme_test()+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text=element_blank())

