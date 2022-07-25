library(tidyverse)
library(magrittr)
library(ggstatsplot)

Bats <- read.csv(file = "Bats_data.csv", header = T, stringsAsFactors = F)

Bats_subset <- select(Bats, Activity, Area.thinned:Distance.creek.water)

rows <- ncol(Bats_subset) - 1

Correlations <- data.frame(
  variable = character(length = rows),
  correlation = numeric(length = rows),
  stringsAsFactors = F
)

for (i in 1:rows) {
  temp1 <- colnames(Bats_subset[i + 1])
  temp2 <- cor(Bats_subset[, 1], Bats_subset[, i + 1], method = "pearson")
  Correlations[i, 1] <- temp1
  Correlations[i, 2] <- temp2
}



read_tsv("data.xls") %>% column_to_rownames(var="TCGA_id") %>% 
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


