library(tidyverse)
library(gggenes)
library(magrittr)
library(ggsci)
library(treeio)
library(ggtree)
library(patchwork)
library(grid)
library(cowplot)
library(ggpubr)
library(gggenomes)
library(MetBrewer)

tree <- read.newick("tree.nwk",node.label = "support") %>%
  ggtree(branch.length = "none")+
  theme_void()+
  theme(legend.title=element_blank(),
        legend.position = c(0.1,1.2), legend.justification = c(0,1.2),
        legend.text=element_text(color="black",size=8))

pfam <- read_tsv("pfam.xls",col_names = F) %>% 
  select(1,4,5,6,12) %>% 
  filter(X12 < 0.001) %>%
  mutate_at(vars("X5"),~str_split(.,"\\.",simplify=TRUE)[,1]) %>% 
  separate(`X5`,into=c("A","B"),sep="-") %>% 
  mutate(A=as.numeric(A)) %>% 
  select(-X12,-B) %>% 
  set_colnames(c("gene","start","end","hmm_acc")) %>% 
  ggplot(aes(xmin = start,xmax = end,y=gene,fill = hmm_acc)) +
  geom_gene_arrow(arrowhead_height = unit(0, "mm"),
                  arrowhead_width = unit(0,"mm"))+
  scale_fill_manual(values=met.brewer("Pissaro", 3))+
  theme_genes()+
  labs(y=NULL)+
  theme(axis.text.y=element_text(color="black",size=8,family="Times",face="italic"),
        axis.text.x=element_text(color="black"),
        legend.title = element_blank(),
        legend.spacing.x=unit(0.1,'cm'),
        legend.key.width=unit(0.4,'cm'),
        legend.position = "top",
        legend.text=element_text(color="black",size=8),
        legend.key.height=unit(0.4,'cm'),
        plot.margin=unit(c(0,0,0,0),units="cm"))+
  guides(fill = guide_legend(direction = "horizontal"))+
  guides(fill=guide_legend(nrow=1, byrow=TRUE))

gene <- gggenomes("ARF.gff") + 
  geom_gene(position="pile",size=3)+
  theme(plot.margin=unit(c(1.1,0,0,0),units="cm"))

g <- gene %>% pick_by_tree(tree) %>% ggplotify::as.grob() %>% ggdraw()
pp <- tree +pfam+plot_layout(widths = c(.6,1))
(pp %>% ggdraw()|g) +theme(plot.margin=unit(c(0,0,0,0),units="cm"))
