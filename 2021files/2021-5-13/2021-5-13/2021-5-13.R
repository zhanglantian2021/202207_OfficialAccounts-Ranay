pacman::p_load(ggstar,ggtree,treeio,aplot,tidyverse,ggthemes,reshape2,
               hablar,patchwork,gggenes,ggseqlogo,ggsci,magrittr)

colors <-c("#E41A1C","#1E90FF","#FF8C00","#4DAF4A","#984EA3",
           "#40E0D0","#FFC0CB","#00BFFF",
           "#FFDEAD","#EE82EE","#00FFFF")

p <- read.delim("gene.xls",header = T) %>%
  as_tibble() %>% convert(num(V2,V3,V4)) %>%
  convert(fct(V5))

gene <- ggplot() +
  geom_gene_arrow(data=p,aes(xmin = V2, xmax = V3, y = V5,
                             forward =V4 ,fill = type),
                  arrowhead_height = unit(3,"mm"),
                  arrowhead_width = unit(0.8, "mm"))+
  geom_segment(data=p,aes(x = V2, xend = V3, y = V5, yend =V5)) +
  facet_wrap(~ V5, scales = "free", ncol = 1) +theme_genes()+
  scale_fill_brewer(palette="Set2")+
  theme(axis.text.x = element_blank(),axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),legend.title = element_blank())+
  labs(x=NULL,y=NULL)

pro <- read.delim("pro.xls",header = T,sep="\t") %>%
  as_tibble() %>% convert(fct(gene)) %>%
  ggplot(aes(xmin = start,xmax = end,y=gene,fill = type)) +
  scale_fill_manual(values = colors)+
  geom_gene_arrow(arrowhead_height = unit(3, "mm"),
                  arrowhead_width = unit(0, "mm"))+
  theme_genes()+ylab(NULL)+
  theme(legend.title = element_blank(),
        axis.text.y=element_blank())

pp2 <- gene + pro +plot_layout(ncol =2,width = c(2.5,2))+
  plot_layout(guides = 'collect')

pp2
