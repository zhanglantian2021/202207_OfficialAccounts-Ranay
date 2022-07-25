library(tidyverse)
library(circlize)
library(ComplexHeatmap)


set.seed(123)
mat1 = rbind(cbind(matrix(rnorm(10*5, mean = 1), nr = 10), 
                   matrix(rnorm(10*5, mean = -1), nr = 10)),
             cbind(matrix(rnorm(10*5, mean = -1), nr = 10), 
                   matrix(rnorm(10*5, mean = 1), nr = 10))
)
rownames(mat1) = paste0("R", 1:20)
colnames(mat1) = paste0("C", 1:10)
mat1 = mat1[sample(20,20), ] # randomly permute rows
split = sample(letters[1:5],20, replace = TRUE)
split = factor(split, levels = letters[1:5])



circos.clear()
circos.par(canvas.xlim=c(-1,1),canvas.ylim=c(-1,1.1),start.degree = 0,
           start.degree = 90, gap.degree =10)

col_fun = colorRamp2(c(-2, 0, 2), c("white", "white", "white"))

col_fun1 = colorRamp2(c(-2,-1,0,1,2),
                      c("#3B9AB2","#0072B5FF","#E18727FF","#20854EFF","#78B7C5"))

mat2 <- mat1 %>% as.data.frame() %>% select(1)

circos.heatmap(mat2, split = split, col = col_fun, track.height =0.0000001, 
               bg.border = "white",show.sector.labels = F)

circos.trackPlotRegion(ylim = c(0,0.1),track.height = 0.08,
                       panel.fun = function(x, y) {
                         chr = CELL_META$sector.index
                         xlim = CELL_META$xlim
                         ylim = CELL_META$ylim
                         circos.text(mean(xlim),mean(ylim),chr,cex = 1,
                                     col = "black",facing = "outside", niceFacing = F)
                         
                       })


circos.heatmap(mat1 %>% as.data.frame() %>% select(1),
               rownames.side = "outside",
               split = split, col = col_fun1, track.height = 0.1,cell_width=0.1,
               show.sector.labels = F)

circos.track(track.index = get.current.track.index(),
             panel.fun = function(x, y) {
               if(CELL_META$sector.numeric.index == 1) {
                 cn = colnames(mat1)
                 n = length(cn)-9
                 circos.text(rep(CELL_META$cell.xlim[2],n) + convert_x(1,"mm"),
                             1:n, cn, col="blACK",
                             cex = 0.6, adj = c(0,2.5),facing = "inside")
               }
             }, bg.border = NA)

circos.track(track.index = get.current.track.index(),
             panel.fun = function(x, y) {
               if(CELL_META$sector.numeric.index == 2) {
                 cn = colnames(mat1)
                 n = length(cn)-9
                 circos.text(rep(CELL_META$cell.xlim[2],n) + convert_x(1,"mm"),
                             1:n, cn, col="blACK",
                             cex = 0.6, adj = c(0,2.5),facing = "inside")
               }
             }, bg.border = NA)

circos.track(track.index = get.current.track.index(),
             panel.fun = function(x, y) {
               if(CELL_META$sector.numeric.index == 4) {
                 cn = colnames(mat1)
                 n = length(cn)-9
                 circos.text(rep(CELL_META$cell.xlim[2],n) + convert_x(1,"mm"),
                             1:n, cn, col="blACK",
                             cex = 0.6, adj = c(0,2.5),facing = "inside")
               }
             }, bg.border = NA)



circos.heatmap(mat1 %>% as.data.frame() %>% select(2),
               split = split, col = col_fun1, track.height = 0.1,cell_width=0.1,
               show.sector.labels = F)


circos.track(track.index = get.current.track.index(),
             panel.fun = function(x, y) {
               if(CELL_META$sector.numeric.index == 4) {
                 cn = colnames(mat1)
                 n = length(cn)-9
                 circos.text(rep(CELL_META$cell.xlim[2],n) + convert_x(1,"mm"),
                             1:n, cn, col="blACK",
                             cex = 0.6, adj = c(0,2.5),facing = "inside")
               }
             }, bg.border = NA)


circos.track(track.index = get.current.track.index(),
             panel.fun = function(x, y) {
               if(CELL_META$sector.numeric.index == 3) {
                 cn = colnames(mat1)
                 n = length(cn)-9
                 circos.text(rep(CELL_META$cell.xlim[2],n) + convert_x(1,"mm"),
                             1:n, cn, col="blACK",
                             cex = 0.6, adj = c(0,2.5),facing = "inside")
               }
             }, bg.border = NA)



circos.heatmap(mat1 %>% as.data.frame() %>% select(2),
               split = split, col = col_fun1, track.height = 0.1,cell_width=0.1,
               show.sector.labels = F)



circos.track(track.index = get.current.track.index(),
             panel.fun = function(x, y) {
               if(CELL_META$sector.numeric.index == 2) {
                 cn = colnames(mat1)
                 n = length(cn)-9
                 circos.text(rep(CELL_META$cell.xlim[2],n) + convert_x(1,"mm"),
                             1:n, cn, col="blACK",
                             cex = 0.6, adj = c(0,2.5),facing = "inside")
               }
             }, bg.border = NA)

text(0, 0, 'rownames.side = "outside"')

lgd = Legend(title = "expression",col_fun = col_fun1, direction = "horizontal",
             legend_width = unit(8,"cm"))

draw(lgd,x = unit(0.5,"npc"),y = unit(0.93,"npc"))

