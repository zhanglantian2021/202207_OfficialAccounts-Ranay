library(tidyverse)
library(AnnotationForge)

emapper <- read.delim("emapper.annotations.xls") %>%
  dplyr::select(GID=query_name,Gene_Symbol=Preferred_name, 
                GO=GOs,KO=KEGG_ko,Pathway =KEGG_Pathway, 
                OG =X.3,Gene_Name =X.4)
gene_info <- dplyr::select(emapper,GID,Gene_Name) %>%
  dplyr::filter(!is.na(Gene_Name))

gene2go <- dplyr::select(emapper,GID,GO) %>%
  separate_rows(GO, sep = ',', convert = F) %>%
  filter(GO!="",!is.na(GO)) %>% 
  mutate(EVIDENCE = 'A')

AnnotationForge::makeOrgPackage(gene_info=gene_info,
                                go=gene2go,
                                maintainer='YJA<yanjunan@gmail.com>',
                                author='YJA',
                                version="0.1" ,
                                outputDir=".", 
                                tax_id="59729",
                                genus="M",
                                species="A",
                                goTable = "go")


# GO富集分析

library(clusterProfiler)
library(org.My.eg.db)


gene <- read.delim("genes.counts.DESeq2.xls") %>%
  filter(abs(log2FoldChange)>1 & padj < 0.05) %>% 
  pull(id)

ego <- enrichGO(gene=gene,OrgDb=org.My.eg.db,keyType="GID",
         ont="ALL",qvalueCutoff = 0.05,pvalueCutoff =0.05)


ego %>% as.data.frame() %>% view()

  group_by(ONTOLOGY) %>% arrange(Count) %>% 
  top_n(10) %>% arrange(ONTOLOGY) %>% 
#  slice(which.max(Count)) %>% 
  ungroup()
  










