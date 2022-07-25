library(tidyverse)
library(ggtree)
library(treeio)
library(ggsci)
library(magrittr)
library(ggtreeExtra)
library(ggnewscale)
library(ggplotify)
library(cowplot)
library(grid)
library(ggpubr)


tree <- read.newick("tree.nwk",node.label = "support")

MRCA(tree,"Ataa14","Ataa15")
MRCA(tree,"Ataa12","Ataa4")
MRCA(tree,"Ataa17","Ataa34")


MRCA(tree,"AtFAB2.3")
MRCA(tree,"Ataa18")


df <- read_tsv("heatmap-2.xls") %>% column_to_rownames(var="X2") %>% 
  t() %>% as.data.frame()


df <- read_tsv("heatmap-2.xls") %>% pivot_longer(-X2)


dt <- read_tsv("dt.xls",col_names = F) %>% as.data.frame() %>% 
  set_colnames(c("ID","value")) %>% 
  mutate(group=rep(c("A","B","C"),times = 3,length.out=43))

p2 <- tree %>% ggtree(branch.length = "none",layout = "circular",
         linetype=1,size=0.5,ladderize = T)+
  geom_point2(aes(subset=!isTip,fill=support),shape=21,size=2,show.legend = F)+
  scale_fill_continuous(low="#ECCBAE",high="#F98400")+
  labs(fill="bootstrap")+layout_fan(angle=180)+
  geom_cladelab(node=44,label="",barcolor="#3B9AB2",extend=0.5,offset=2.5,barsize=15,alpha=0.1)+
  geom_cladelab(node=71,label="",barcolor="#78B7C5",extend=0.5,offset=2.5,barsize=15,alpha=0.5)+
  geom_cladelab(node=49,label="",barcolor="#046C9A",extend=0.5,offset=2.5,barsize=15,alpha=0.5)+
  geom_tiplab(size=3,family="Times",fontface="italic",color="black",offset=.5)+
  new_scale_fill() +
  geom_fruit(data=df,offset = 0.23,geom=geom_tile,mapping=aes(y=name,x=X2,fill=value),
             pwidth=0.8,axis.params=list(axis="x", text.angle=-90,hjust=0,text.size =2))+
  scale_fill_gradient2(mid="#78B7C5",low="#0C6291",high="#A63446") +
  new_scale_fill() +
  geom_fruit(data=dt,geom=geom_bar,mapping=aes(y=ID, x=value,fill=group),
             orientation="y",offset=.1, pwidth=.6,stat="identity")+
  scale_fill_manual(values=c("#3B9AB2","#78B7C5","#046C9A"),
    guide=guide_legend(keywidth=1, keyheight=1, order=3))+
  theme(legend.position = "top")

p3 <- p2 + theme(legend.position = "non",
                 plot.margin = unit(c(0,0,-7,0),units="cm"))

p3 %>% ggdraw()+ 
  draw_plot(ggpubr::get_legend(p2) %>% as_ggplot(),scale =1,x=0,y=-.3)
  
