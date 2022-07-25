library(tidyverse)
library(circlize)
library(stringr)
library(dendextend)
library(ComplexHeatmap)

df <- read_tsv("genes.counts.DESeq2.xls") %>%
  mutate(gene_type=if_else(
    padj > 0.05,"ns",if_else(
      abs(log2FoldChange) < 1,"ns",
      if_else(log2FoldChange >=1,"up","down"))),
    FC=2**log2FoldChange) %>% 
  filter(gene_type=="up",FC>8) %>% 
  mutate(across("id",str_replace,"TF","HF")) %>% 
  select(1) %>% 
  left_join(.,read.delim("genes.TMM.xls") %>% 
              dplyr::rename(id="X"),by="id") %>% 
  column_to_rownames("id") %>% 
  mutate_if(is.numeric,function(x) x+1) %>% 
  log10() %>% 
  select(1,2,3,4,5,6,7,8,9) %>%
  rename_at(vars(starts_with("BLO_")),funs(str_remove(.,"BLO_")))

split = sample(letters[1:4],95, replace = TRUE)
split = factor(split, levels = letters[1:4])


col_fun1 = colorRamp2(c(0,1,2,3,4,5),
                      c("#3B9AB2","#0072B5FF","#E18727FF","#20854EFF",
                        "#00A08A","#78B7C5"))

circos.par(gap.after=c(1,1,1,20))
circos.heatmap(df[,1:3],col = col_fun1,rownames.side = "outside",split = split,
               cluster = T,track.height =0.1)

circos.track(track.index = get.current.track.index(),
             panel.fun = function(x, y) {
               if(CELL_META$sector.numeric.index == 4) {
                 cn = colnames(df[,1:3])
                 n = length(cn)
                 circos.text(rep(CELL_META$cell.xlim[2],n) + convert_x(0.8,"mm"), 
                             1:n, cn, 
                             cex = 0.5, adj = c(0,0.5),facing = "inside")
               }
             }, bg.border = NA)

circos.heatmap(df[,4:6],col = col_fun1,cluster = T,track.height =0.1)

circos.track(track.index = get.current.track.index(),
             panel.fun = function(x, y) {
               if(CELL_META$sector.numeric.index == 4) {
                 cn = colnames(df[,4:6])
                 n = length(cn)
                 circos.text(rep(CELL_META$cell.xlim[2],n) + convert_x(0.8,"mm"), 
                             1:n, cn, 
                             cex = 0.5, adj = c(0,0.5),facing = "inside")
               }
             }, bg.border = NA)

circos.heatmap(df[,7:9],col = col_fun1,cluster = T,track.height =0.1)

circos.track(track.index = get.current.track.index(),
             panel.fun = function(x, y) {
               if(CELL_META$sector.numeric.index == 4) {
                 cn = colnames(df[,7:9])
                 n = length(cn)
                 circos.text(rep(CELL_META$cell.xlim[2],n) + convert_x(0.8,"mm"), 
                             1:n-0.5, cn, 
                             cex = 0.5, adj = c(0,0.5),facing = "inside")
               }
             }, bg.border = NA)

circos.track(track.index = get.current.track.index(), panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$cell.ylim[2] + convert_y(34,"mm"), 
              paste0("group ", CELL_META$sector.index),
              facing = "bending.inside", cex = 0.8,
              adj = c(0.5, 0), niceFacing = TRUE)
  }, bg.border = NA)

circos.rect(CELL_META$cell.xlim[2] + convert_x(17, "mm"), 15,
            CELL_META$cell.xlim[2] + convert_x(82.5, "mm"), 16,
            col = "#3C5488FF", border = NA)

circos.rect(CELL_META$cell.xlim[2] + convert_x(82.5, "mm"), 15,
            CELL_META$cell.xlim[2] + convert_x(156.5, "mm"), 16,
            col = "#00A087FF", border = NA)

circos.rect(CELL_META$cell.xlim[2] + convert_x(156.5, "mm"), 15,
            CELL_META$cell.xlim[2] + convert_x(225.5, "mm"), 16,
            col = "#4DBBD5FF",border = NA)

circos.rect(CELL_META$cell.xlim[2] + convert_x(225.5, "mm"), 15,
            CELL_META$cell.xlim[2] + convert_x(279, "mm"), 16,
            col ="#E64B35FF", border = NA)

lgd = Legend(title = "expression",col_fun = col_fun1)

draw(lgd,x = unit(0.55,"npc"),y = unit(0.5,"npc"))

circos.clear()





