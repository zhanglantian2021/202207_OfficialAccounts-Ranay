library(GEOquery)
library(tidyverse)

gset <- getGEO("GSE33126",getGPL = FALSE)
Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 5)
gset <- getGEO("GSE33126",getGPL = FALSE)
save(gset,file="GSE33126.rdata")

load(file ="GSE33126.rdata")

gset <- gset[[1]]
class(gset)

# ExpressionSet 包含表达矩阵与样本信息表

sampleinfo <- pData(gset) %>%# 提取样本信息表
  select(source_name_ch1,characteristics_ch1.1) %>% 
  rename(group = source_name_ch1,patient=characteristics_ch1.1) %>% 
  mutate_at(vars(patient),~str_split(.," ",simplify = T)[,2])

gene_exp <- exprs(gset)


>以下部分为Python代码
import os
os.chdir("~/Desktop/TCGA/DataMiner-main")
from tcga_downloader import *
  ids=get_ids('gdc_manifest.txt')
payload=prepare_payload(ids,data_type='Gene Expression Quantification')
metadata=get_metadata(payload)
download_data(metadata,sep="\t",outdir="BRCA")
