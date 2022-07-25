library(tidyverse)
library(ggtree)
library(treeio)
library(ggsci)

tree <- read.newick("tree.nwk",node.label = "support")
group <- read.table("group.xls",header = T,row.names = 1)
group1 <- split(row.names(group),group$Group)


pp3 <- groupOTU(tree,group1) %>%
  ggtree(branch.length = "none",layout = "circular",
         linetype=1,size=0.8,ladderize = T,aes(color=group))+
  #geom_text(aes(label=node), hjust=-3,size=3) +
  geom_tiplab(size=3,family="Times",hjust = -0.1)+
  scale_color_nejm()+labs(color="")+
  geom_hilight(node=63,fill="#1E90FF",type="rect",alpha=0.6)+
  geom_hilight(node=36,fill="#FF8C00",type="rect",alpha=0.6)+
  geom_hilight(node=43,fill="#4DAF4A",type="rect",alpha=0.6)+
  geom_hilight(node=47,fill="#984EA3",type="rect",alpha=0.6)+
  geom_point2(aes(subset=!isTip,fill=support),shape=21,size=2)+
  scale_fill_continuous(low='green',high='red')+
  geom_strip(62,49,label = "Group I", align = T, alpha=.8,family="Times",
             fontsize=5,offset = 7, color = "#984EA3",offset.text = 2 ,
             hjust="center",barsize = 5,extend = 1)+
  geom_strip(44,46,label = "Group II", align = T, alpha=.8,family="Times",
             fontsize=5,offset = 7, color ="#4DAF4A",offset.text = 2 ,
             hjust="center",barsize = 5,extend = 1,angle=66)+
  geom_strip(40,1,label = "Group III", align = T, alpha=.8,family="Times",
             fontsize=5,offset = 7, color = "#FF8C00",offset.text = 2 ,
             hjust="center",barsize = 5,extend = 1,angle=0)+
  geom_strip(63,66,label = "Group V", align = T, alpha=.8,family="Times",
             fontsize=5,offset = 7, color = "#1E90FF",offset.text = 2 ,
             hjust="center",barsize = 5,extend = 1,angle=-50)+
  labs(fill = "bootstrap")
ggsave(pp3,file="tree.png",width =8,height =7,units="in",dpi=300)

pp3

library(ggpubr)
tibble(函数= c("geom_strip", "geom_tiplab()","geom_fruit()",
             "offset.text","extend","offset","hjust=center"),
         功能= c("根据节点添加外部条带,后跟节点位置信息", 
               "设置标签显示","绘制条形","调整label位置",
               "调整条带之间间距","设置距离节点的位置","将lable居中放置")) %>%
  ggtexttable(rows = NULL, theme = ttheme("mBlue"))
