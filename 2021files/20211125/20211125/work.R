library(circlize)
library(tidyverse)

bed <- generateRandomBed(nr=50,nc=4) %>% 
  mutate(A1 = rep(LETTERS[1:5], times=c(11,11,11,11,11)),
         ID = paste0(A1, 1:5)) %>% dplyr::select(-A1)

circos.clear()
circos.par(canvas.xlim=c(-1,1),canvas.ylim=c(-1,1),start.degree = 0)
circos.initializeWithIdeogram(plotType = NULL)

circos.genomicLabels(bed,labels.column =8,side = "outside",
                     connection_height= mm_h(4), 
                     labels_height=0.1,
                     cex=0.6,
                     labels.side = side,
                     col = as.numeric(factor(bed[[8]])) %>% 
                       as.data.frame() %>% 
                       filter(.%in% c(2,10,15,17,19,30)) %>% pull())

set_track_gap(mm_h(0.3))

circos.trackPlotRegion(ylim = c(0,0.1),track.height = 0.05,bg.border="white", 
                       panel.fun = function(x, y) {
                         chr = CELL_META$sector.index
                         xlim = CELL_META$xlim
                         ylim = CELL_META$ylim
                         circos.text(mean(xlim),mean(ylim),chr,cex = 0.5,
                                     col = "black",facing = "outside", niceFacing = TRUE)
                         
                       })

circos.genomicIdeogram(track.height = mm_h(2))

circos.genomicHeatmap(bed %>% dplyr::select(1,2,3,4,5,6,7),
                      side = "inside", # 热图方向
                      heatmap_height = 0.1,  # 热图轨道高度
                      connection_height =mm_h(6),  #连接线的高度
                      border = "white",track.margin=c(0.03,0))

circos.genomicHeatmap(bed %>% dplyr::select(1,2,3,4,5,6,7),
                      side = "inside", 
                      connection_height=NULL, # NULL不显示连接线
                      heatmap_height = 0.1,border = "white",track.margin=c(0.03,0))

# track.margin 定义轨道下部与上部间距
region1 = generateRandomBed(nr = 1000);
region1 = region1[sample(nrow(region1), 20), ]

region2 = generateRandomBed(nr = 1000); 
region2 = region2[sample(nrow(region2), 20), ]

circos.genomicLink(region1, region2,col = sample(10,20,replace = TRUE))

highlight.chromosome(c("chr1","chr12"),col="#0000FF40",border = "green")

circos.clear()



