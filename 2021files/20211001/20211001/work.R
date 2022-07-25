library(tidyverse)
library(stringr)
library(circlize)
library(ComplexHeatmap)

load("data.RData")

col_fun = colorRamp2(c(-5,0,5), c("blue","white","red"))
col_fun(seq(-5,5, by=2.5))

df <- dd %>% as.data.frame() %>% 
  separate_rows(.,geneID,convert=TRUE,sep="/") %>%
  left_join(.,geneList %>% as.data.frame() %>% dplyr::rename(FC=".") %>%
              rownames_to_column(var="geneID"),by="geneID") %>%
  select(2,geneID,Count,FC) %>%
  filter(Description %in% c("Photosynthesis","Photosynthesis - antenna proteins","Fatty acid metabolism")) %>%
  mutate(col=case_when(FC <= 5 & FC > 2.5 ~ "#FF0000FF",
                       FC <= 2.5 & FC > 0 ~ "#FF9E81FF",
                       FC <= 0 & FC > -2.5 ~ "#FFFFFFFF",
                       FC <= -2.5 & FC >-5 ~ "#B38BFFFF",
                       FC <= -5 ~ "#0000FFFF")) %>% 
  filter(geneID %in% c("TF43612","TF22752","TF17006",
                       "TF44607","TF39252","TF13897","TF39451","TF12653","TF39927",
                       "TF24170","TF39128","TF27727","TF05765","TF22149","TF25141",
                       "TF28686","TF39137","TF37077","TF17235","TF32011","TF18226",
                       "TF20321","TF44063","TF15114","TF29223","TF05356","TF37048",
                       "TF34750","TF15436","TF39134","TF06590","TF35714","TF42713")) %>% 
  mutate(size=1) %>% relocate(size,.after=geneID) %>% 
  mutate(Description=case_when(Description=="Photosynthesis" ~ "Ko00195",
                               Description=="Photosynthesis - antenna proteins" ~ "Ko00196",
                               Description=="Fatty acid metabolism" ~ "Ko01212"))


grid.col = c(Ko00195="#0073C2FF",
             Ko00196="#4DBBD5FF",
             Ko01212="#00A087FF",
              TF43612="#FF9E81FF",TF22752="#FF9E81FF",TF17006="#FF9E81FF",
              TF44607="#FF9E81FF",TF39252="#FF9E81FF",TF13897="#FF9E81FF",
              TF39451="#FF9E81FF",TF12653="#FF9E81FF",TF39927="#FF9E81FF",
              TF24170="#FF9E81FF",TF39128="#FF0000FF",TF27727="#FF0000FF",
              TF05765="#FF0000FF",TF22149="#FF0000FF",TF25141="#FF9E81FF",
              TF28686="#FF0000FF",TF39137="#FF9E81FF",TF37077="#FF9E81FF",
              TF17235="#FF9E81FF",TF32011="#FF9E81FF",TF18226="#FF9E81FF",
              TF20321="#FF0000FF",TF44063="#FF9E81FF",TF15114="#FF9E81FF",
              TF29223="#FF9E81FF",TF05356="#FF9E81FF",TF37048="#FF9E81FF",
              TF34750="#FF9E81FF",TF15436="#FFFFFFFF",TF39134="#FF9E81FF",
              TF06590="#FF0000FF",TF35714="#FF9E81FF",TF42713="#FF0000FF")

circos.clear()

circos.par(canvas.xlim=c(-1,1.2),canvas.ylim=c(-1.2,1.1),start.degree = 0)

chordDiagram(df[,1:3],link.sort = T,link.decreasing = TRUE, 
             transparency =0.3,grid.col = grid.col,
             annotationTrack ="grid",
             big.gap = 20,
             directional = -1, 
             diffHeight = mm_h(4),
             target.prop.height = mm_h(2),
             preAllocateTracks = list(track.height = 0.01),
             direction.type = c("diffHeight","arrows"),link.arr.type = "big.arrow")

for(si in get.all.sector.index() %>% as.data.frame() %>% 
    dplyr::slice(4:n()) %>% 
    pull()) {
  xlim = get.cell.meta.data("xlim",sector.index = si,track.index = 1)
  ylim = get.cell.meta.data("ylim",sector.index = si,track.index = 1)
  circos.text(mean(xlim), ylim[1],labels = si,sector.index = si,
              track.index = 1, 
              facing = "clockwise", 
              cex=0.5,
              adj=c(0,.5),
              niceFacing = T)
}

circos.track(track.index = 2,panel.fun = function(x, y) {
  if(abs(CELL_META$cell.start.degree - CELL_META$cell.end.degree) > 5) {
    sn = CELL_META$sector.index
    circos.text(CELL_META$xcenter,CELL_META$ycenter,sn,col = "white", 
                font=1, cex =0.7, adj = c(0.5,0.5), niceFacing = TRUE)
    xlim = CELL_META$xlim
    breaks = seq(0, xlim[2], by = 4e5)
  }
}, bg.border = NA)


col_fun3 = colorRamp2(c(50,19,30),c("#0073C2FF","#00A087FF","#4DBBD5FF"))
lgd3 = Legend(col_fun = col_fun3,at =c(19,30,50),title="Count")
draw(lgd3,x = unit(0.9,"npc"),y=unit(0.5,"npc"),just = c("right","top"))

col_fun2=colorRamp2(c(-5,-2.5,0,2.5,5),c("#0000FFFF","#B38BFFFF","#FFFFFFFF","#FF9E81FF","#FF0000FF"))
lgd2 = Legend(col_fun = col_fun2,at =c(-5,-2.5,0,2.5,5),title="fold change")
draw(lgd2,x = unit(0.95,"npc"),y=unit(0.7,"npc"),just = c("right","top"))

lgd = Legend(labels=c("Ko00195 [Photosynthesis]",
                      "Ko00196 [Photosynthesis - antenna proteins]",
                      "Ko01212 [Fatty acid metabolism]"),
             legend_gp = gpar(fill=c("#0073C2FF","#4DBBD5FF","#00A087FF")),nr=3)

pd = packLegend(lgd,max_width = unit(1,"cm"), 
                direction = "horizontal", column_gap = unit(5,"mm"),row_gap = unit(1, "cm"))

pushViewport(viewport(width=0.8,height = 0.8))

draw(pd,x= unit(9,"cm"), y = unit(-0.5,"cm"))

circos.rect(CELL_META$cell.xlim[2] + convert_x(200, "mm"),4.5,
            CELL_META$cell.xlim[2] + convert_x(390, "mm"),5,
            col ="#00A087FF", border = NA)


