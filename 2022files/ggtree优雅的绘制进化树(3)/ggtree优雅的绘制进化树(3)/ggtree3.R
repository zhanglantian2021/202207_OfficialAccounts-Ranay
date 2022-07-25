library(ggtreeExtra)
library(ggtree)
library(treeio)
library(tidyverse)
library(ggstar)
library(ggnewscale)

df <- read.delim("genus.xls",sep="\t",row.names = 1) 

dat2 <- df %>% mutate_if(is.numeric,function(x) x+ 1) %>%
  log10() %>% 
  rownames_to_column(var="ID") %>% pivot_longer(-ID) %>% 
  mutate(name = trimws(str_remove(name,"(\\s+[A-Za-z]+)?[3-6-]+")))

dat3 <- dat2 %>% arrange(desc(value)) %>% group_by(ID) %>% 
  filter(row_number()==1) %>% ungroup() %>% 
  mutate(name = trimws(str_remove(name,"(\\s+[A-Za-z]+)?[0-9-]+"))) %>% 
  sample_frac(.3)
         
p <- hclust(dist(df)) %>% ggtree(layout="fan", open.angle=10)

p + new_scale_fill() +
  geom_fruit(data=dat2, geom=geom_tile,
             mapping=aes(y=ID, x=name,alpha=value,fill=name),
             color = "grey50",offset = 0.04,size = 0.02)+
  scale_fill_manual(values=c("#FFC125","#87CEFA","#7B68EE","#808080",
                             "#800080", "#9ACD32","#D15FEE","#FFC0CB",
                             "#EE6A50","#8DEEEE", "#006400","#800000",
                             "#B0171F","#191970"))+
  scale_alpha_continuous(guide=NULL)+
  new_scale_fill()+
  geom_fruit(data=dat3,geom=geom_bar,
             mapping=aes(y=ID, x=value,fill=name),
             pwidth=0.38, 
             orientation="y", 
             stat="identity",
  )+
  scale_fill_manual(values=c("#E64B35FF","#4DBBD5FF","#00A087FF","#3C5488FF","#F39B7FFF","#8491B4FF"))+
  theme(legend.position=c(0.45, 0.5),
        legend.background=element_rect(fill=NA),
        legend.title=element_blank(),
        legend.text=element_text(size=10)
  )

