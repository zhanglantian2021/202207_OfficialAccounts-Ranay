library(tidyverse)
library(magrittr)
library(clusterProfiler)

keggannotation <- read_tsv("pathway",col_names = F) %>% 
  left_join(.,read_tsv('map.txt',col_names = F),by="X1") %>% 
  select(-1) %>% set_colnames(c("pathway","ID")) %>% 
  mutate(across("ID",str_replace,"cpd:","")) %>% select(2,1) %>% 
  arrange(ID)

allkeggid <- read_tsv("meta_intensity_neg.anno.xls") %>% select(KEGG) %>% 
  bind_rows(.,read_tsv("meta_intensity_pos.anno.xls") %>% select(KEGG)) %>% 
  arrange() %>% filter(KEGG !="_") %>% set_colnames(c("ID")) 

diffkeggID <- read_tsv("diff.xls") %>% select(KEGG) %>% 
  arrange() %>% filter(KEGG !="_") %>% set_colnames(c("ID"))

total <- right_join(keggannotation,allkeggid,by="ID") %>% select(2,1)

x <- clusterProfiler::enricher(gene = diffkeggID$ID,TERM2GENE = total,minGSSize = 1,pvalueCutoff = 1,qvalueCutoff = 1)

write.csv(as.data.frame(x@result) %>% select(-1,-2),file="KEGG_enrichment_result.csv")

df <- read_csv("KEGG_enrichment_result.csv") %>%
  dplyr::rename("Description"="...1") %>% 
  arrange(desc(Count)) %>%
  select(1,2,3,4,8) %>% 
  separate(`GeneRatio`,into=c("A","B"),sep="/") %>%
  mutate(A=as.numeric(A),B=as.numeric(B)) %>%
  mutate(count=A/B) %>% head(30) %>% arrange(Count)

df$Description <- factor(df$Description,levels = c(df$Description %>% as.data.frame() %>% pull()))

df %>% ggplot(aes(count,Description))+
  geom_point(aes(size=Count,color=pvalue,fill=pvalue),pch=21)+
  scale_color_gradientn(colours = (rev(RColorBrewer::brewer.pal(11,"RdBu"))))+
  scale_fill_gradientn(colours =(rev(RColorBrewer::brewer.pal(11,"RdBu"))))+
  guides(size=guide_legend(title="Count"))+
  labs(x=NULL,y=NULL)+
  theme(axis.title = element_blank(),
        axis.text.x=element_text(color="black",angle =0,hjust=0.5,vjust=0.5, margin = margin(b =5)),
        axis.text.y=element_text(color="black",angle =0,hjust=1,vjust=0.5),
        panel.background = element_rect(fill = NA,color = NA),
        panel.grid.minor= element_line(size=0.2,color="#e5e5e5"),
        panel.grid.major = element_line(size=0.2,color="#e5e5e5"),
        panel.border = element_rect(fill=NA,color="black",size=1,linetype="solid"),
        legend.key=element_blank(),
        legend.title = element_text(color="black",size=9),
        legend.text = element_text(color="black",size=8),
        legend.spacing.x=unit(0.1,'cm'),
        legend.key.width=unit(0.5,'cm'),
        legend.key.height=unit(0.5,'cm'),
        legend.background=element_blank(),
        legend.box="horizontal",
        legend.box.background = element_rect(color="black"),
        legend.position = c(1,0),legend.justification = c(1,0))+
  scale_y_discrete(labels = function(y) str_wrap(y,width=30))



