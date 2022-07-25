library(tidyverse)
library(circlize)

df <- read.delim("otu.xls",check.names = F) %>%
  dplyr::rename(otu=`OTU ID`) %>% 
  separate(taxonomy,
           into=c("domain","phylum","class",
                  "order","family","genus","species"),sep=";") %>%
   select(CAKB1:CAIF6,genus) %>% 
   drop_na() %>% 
   rowwise() %>%
   mutate(sum = sum(across(where(is.numeric)))) %>%
   arrange(desc(sum)) %>% 
   head(10) %>% 
   select(-sum) %>% pivot_longer(-genus) %>% 
   mutate_at(vars(-name,-value),~str_split(.,"__",simplify=TRUE)[,2]) %>%
   filter(genus != "uncultured") %>% 
   dplyr::rename(Sample=name) %>% 
   left_join(.,read.delim("group.xls"),by="Sample") %>% 
   select(-Sample) %>% select(genus,Group,value)
df

circos.par(canvas.xlim=c(-1,1),canvas.ylim=c(-1,1.2),start.degree = 0)

set.seed(1234)
chordDiagram(df,
             link.sort = FALSE, 
             link.decreasing = TRUE, 
             transparency = 0.1, 
             annotationTrack = "grid",
             preAllocateTracks = list(track.height = .1))

for(si in get.all.sector.index()) {
  xlim = get.cell.meta.data("xlim",sector.index = si,track.index = 1)
  ylim = get.cell.meta.data("ylim",sector.index = si,track.index = 1)
  circos.text(mean(xlim), ylim[1],labels = si,sector.index = si,
              track.index = 1, 
              facing = "clockwise", 
              cex=0.8,
              adj=c(0,.5),
              niceFacing = T)
}

circos.clear()

