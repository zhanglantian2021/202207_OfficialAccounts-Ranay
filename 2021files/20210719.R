library(tidyverse)
library(circlize)
library(ComplexHeatmap)

set.seed(123)


bed <- generateRandomBed(nr=20,fun = function(k) sample(letters,k,replace = TRUE)) %>% 
  rename(gene=value1)

bed1 <- generateRandomBed(nr=50,nc =3) %>%
  filter(value1 > 0 | value2 > 0.2& value3 < -0.1) %>%
  head(20)



circos.initializeWithIdeogram(plotType = NULL)
circos.genomicLabels(bed, labels.column = 4, side = "outside",
                     col = as.numeric(factor(bed[[1]])),
                     line_col = as.numeric(factor(bed[[1]])))
circos.genomicIdeogram()


col_fun1 = colorRamp2(c(-1,0,0.5,1),
                      c("#3B9AB2","#78B7C5","#EBCC2A","#E1AF00"))
col_fun2 = colorRamp2(c(-1,0,1),
                      c("#FF0000","#00A08A","#F2AD00"))
col_fun3 = colorRamp2(c(-1,0,0.5,1),
                      c("#FF0000","#00A08A","#F2AD00","#F21A00"))

circos.genomicHeatmap(bed1 %>% select(1,2,3,4),col = col_fun1,side = "inside", # 热图方向
                      heatmap_height = 0.2,  # 热图轨道高度
                      connection_height =mm_h(10),  #连接线的高度
                      border = "white")

circos.genomicHeatmap(bed1 %>% select(1,2,3,5),col = col_fun2, 
                      side = "inside",
                      connection_height=NULL, # NULL不显示连接线
                      heatmap_height = 0.15,
                      border = "white")

circos.genomicHeatmap(bed1 %>% select(1,2,3,6),col = col_fun3, 
                      side = "inside",
                      connection_height=NULL,
                      heatmap_height = 0.15,
                      border = "white")

lgd1 = Legend(col_fun = col_fun1,title="group1",at =c(-1,0,0.5,1))
draw(lgd1,x = unit(0.9,"npc"), y = unit(0.8,"npc"),just = c("right","top"))

lgd2 = Legend(col_fun = col_fun2,title="group2",at =c(-1,0,1))
draw(lgd2,x = unit(0.9,"npc"), y = unit(0.6,"npc"),just = c("right","top"))

lgd3 = Legend(col_fun = col_fun3,title="group3",at =c(-1,0,0.5,1))
draw(lgd3,x = unit(0.9,"npc"), y = unit(0.4,"npc"),just = c("right","top"))