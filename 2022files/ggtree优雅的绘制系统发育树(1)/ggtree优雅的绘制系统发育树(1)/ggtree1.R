library(tidyverse)
library(ggtree)
library(treeio)
library(ggsci)

tree <- read.newick("tree.nwk",node.label = "support")

MRCA(tree,"Ataa14","Ataa15")
MRCA(tree,"Ataa12","Ataa4")
MRCA(tree,"Ataa17","Ataa34")


MRCA(tree,"Ataa14")
MRCA(tree,"Ataa15")

MRCA(tree,"Ataa12")
MRCA(tree,"Ataa4")

MRCA(tree,"Ataa17")
MRCA(tree,"Ataa34")


tree %>% ggtree(branch.length = "none",layout = "circular",
                      linetype=1,size=0.5,ladderize = T)+
  geom_point2(aes(subset=!isTip,fill=support),shape=21,size=2,show.legend = F)+
  scale_fill_continuous(low="#ECCBAE",high="#F98400")+
  geom_cladelab(node=44,label="",barcolor="#3B9AB2",extend=0.5,offset=1.2,barsize=15,alpha=0.1)+
  geom_cladelab(node=71,label="",barcolor="#78B7C5",extend=0.5,offset=1.2,barsize=15,alpha=0.5)+
  geom_cladelab(node=49,label="",barcolor="#046C9A",extend=0.5,offset=1.2,barsize=15,alpha=0.5)+
  geom_tiplab(size=3,family="Times",fontface="italic",color="black",offset=.5)+
  scale_fill_continuous(low="#ECCBAE",high="#F98400")+
  labs(fill="bootstrap")+
  layout_fan(angle=50)+
  theme(legend.position = "non")+
  geom_strip(1,43,label="Group I",offset=4,barsize =2,extend=0.5,color="#DC0000FF",
             hjust="center",align=T,offset.text=1.2,angle=-60)+
  geom_strip(28,39,label="Group II",offset=4,barsize =2,extend=0.5,color="#3C5488FF",
             hjust="center",align=T,offset.text=1.2,angle=5)+
  geom_strip(5,23,label="Group III",offset=4,barsize =2,extend=0.5,color="#00A087FF",
             hjust="center",align=T,offset.text=1.2,angle=-32)

tree %>% ggtree(branch.length = "none",layout = "circular",
                linetype=1,size=0.5,ladderize = T)+
  geom_tiplab(size=3,family="Times",fontface="italic",color="black",offset=.5)
