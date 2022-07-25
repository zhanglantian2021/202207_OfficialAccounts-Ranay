library(stringr)
library(clusterProfiler)
library(magrittr)
library(tidyverse)

# 1. 提取出差异基因 

gene <- read.delim("genes.counts.DESeq2.xls") %>%
  filter(abs(log2FoldChange)>1 & padj < 0.05) %>% 
  pull(id)

# 2.读取基因功能注释的表格

gene_pathway <- read.delim('query_seqs.fa.emapper.annotations.xls',
                           sep="\t",check.names=F) %>%
  dplyr::select(1,KEGG_ko,KEGG_Pathway) %>% 
  separate_rows(KEGG_Pathway,sep=',',convert = F) %>%
  filter(str_detect(KEGG_Pathway,'ko')) %>%
  mutate(KEGG_ko=str_remove(KEGG_ko,"ko:")) %>%
  select(3,1)
gene_pathway


pathway_name <- clusterProfiler:::kegg_list("pathway") %>%
  mutate(across("from",str_replace,"path:map","ko")) %>% 
  set_colnames(c("path_id","path_name"))

dd <- enricher(gene,
                   TERM2GENE = gene_pathway,
                   TERM2NAME = pathway_name,
                   pvalueCutoff = 0.05,
                   qvalueCutoff = 0.05)
dd %>% as.data.frame() %>% view()


# 翻译单个Ko
bitr_kegg("K03043","kegg", "Path", "ko") -> x
ko2name(x$Path) -> y
merge(x, y, by.x='Path', by.y='ko')
