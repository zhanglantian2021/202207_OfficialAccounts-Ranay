library(tidyverse)
library(magrittr)
library(ggstatsplot)

df_cor <- read_tsv("data.xls") %>% column_to_rownames(var="TCGA_id") %>% 
  pivot_longer(-B2M) %>% 
  pivot_longer(names_to = "name_2", values_to = "value_2",B2M) %>%
  group_by(name_2,name) %>% 
  summarise(cor= cor.test(value_2,value,method="spearman")$estimate,
            p.value = cor.test(value_2,value,method="spearman")$p.value) %>% 
  set_colnames(c("gene_1","gene_2","cor","pvalue")) %>% 
  filter(pvalue < 0.05) %>% 
  arrange(desc(abs(cor)))%>% 
  dplyr::slice(1:500)
  

df2 <- read_tsv("data.xls")

ggscatterstats(data = df2,y = B2M,x=APOBEC3H,
               centrality.para = "mean",
               margins = "both",                                         
               xfill = "#CC79A7", 
               yfill = "#009E73", 
               marginal.type = "histogram")


make_plot <- function(data,x,y){
  ggscatterstats(data,x={{x}},y={{y}},
                 centrality.para = "mean",
                 margins = "both",                                         
                 xfill = "#CC79A7", 
                 yfill = "#009E73", 
                 marginal.type = "histogram")
}

make_plot(df2,B2M,SSTR3)

gene <- df_cor %>% ungroup() %>% select(2) %>% head(10) %>% pull()


plot_list <- vector('list',length(gene))

for (i in seq_along(gene)) {
  plot_list[[i]] <- make_plot(df2,B2M,!!sym(gene[i]))
#  print(plot_list[[i]])
  ggsave(plot_list[[i]],file=paste0("B2M_",gene[i],".pdf"))
}

