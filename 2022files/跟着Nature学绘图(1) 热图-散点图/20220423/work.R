library(tidyverse)
library(corrplot)
library(RColorBrewer)
library(ggsci)
library(ggplotify)
library(patchwork)
library(grid)
library(cowplot)
library(ggpubr)

df <- read_tsv("data.xls") %>% column_to_rownames(var="gene") %>% t() %>% 
  as.matrix()

corrplot(df,method="pie",is.corr=FALSE, tl.col = "black",mar = c(0,0,1.5,0),
         cl.pos = 'b',tl.cex=0.7,cl.ratio=0.4,cl.length=6,cl.cex=0.7,
         col= rev(RColorBrewer::brewer.pal(6,"RdBu")))

read_tsv("data2.txt") %>% select(cell:`High 95% CI`) %>% 
  mutate(col=case_when(p_value > 0.05 ~ "N",
                       p_value < 0.05 ~ "Y")) %>% 
  ggplot(aes(cancer,`Odds Ratio`,ymin=`Low 95% CI`,ymax=`High 95% CI`))+
#  geom_pointrange(color="black",shape=20)+
  geom_errorbar(aes(ymin=`Low 95% CI`,ymax=`High 95% CI`), width = 0.5)+
  geom_point(aes(color=col,fill=col),pch=21,size=3)+
  geom_hline(yintercept =1,linetype ="dashed") + 
  coord_flip()+
  facet_grid(~cell,scales = "free_x")+
  scale_fill_jco()+
  scale_color_jco()+
  theme_test()+
  theme(panel.spacing.x = unit(0.2,"cm"),
        panel.spacing.y = unit(0.1, "cm"),
        axis.title = element_blank(),
        axis.line = element_line(color = "#999999",size = 0.2),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.2,color = "#e5e5e5"),
        strip.text.x = element_text(size=11,color="black"),
        strip.background = element_blank(),
        axis.text = element_text(color="black",size=9),
        legend.position = "non",
        plot.margin=unit(c(0.3,0.3,0.3,0.3),units=,"cm"))


