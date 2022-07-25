library(GEOquery)
library(tidyverse)
library(magrittr)


load(file ="GSE33126.rdata")

gset <- gset[[1]]
class(gset)

# ExpressionSet 包含表达矩阵与样本信息表

sampleinfo <- pData(gset) %>%# 提取样本信息表
  select(source_name_ch1,characteristics_ch1.1) %>% 
  rename(group = source_name_ch1,patient=characteristics_ch1.1) %>% 
  mutate_at(vars(patient),~str_split(.," ",simplify = T)[,2])
  
gene_exp <- exprs(gset)


gene_exp %<>% as.data.frame()


# 检查探针编号
tail(gene_exp[,1:3])

# 同步表达矩阵

gene_exp <- gene_exp[,which(
  colnames(gene_exp) %in% rownames(sampleinfo)
)]

summary(gene_exp)


boxplot(log2(gene_exp),outline=FALSE)

#-------------------------
#  对数据进行标准化
library(limma)
p <- as.data.frame(
  normalizeBetweenArrays(gene_exp)
)

boxplot(p,outline=FALSE)
# 基因信息表

gene_info <- read_delim("GPL6947-13512.txt", "\t",escape_double = FALSE, comment = "#", 
                        trim_ws = TRUE) %>%
  dplyr::select(ID,Gene_Symbol = Symbol,Entrez_Gene_ID, 
                Gene_Title = Definition) %>% drop_na()


save(gene_exp,sampleinfo,gene_info, file='GSE33126-info.rdata')
