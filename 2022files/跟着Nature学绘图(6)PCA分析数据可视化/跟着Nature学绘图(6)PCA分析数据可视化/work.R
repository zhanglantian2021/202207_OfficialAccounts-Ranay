pacman::p_load(tidyverse,ggrepel,FactoMineR,magrittr,factoextra,RColorBrewer)

df <- read_tsv("F3.xls")

pca <- df %>% column_to_rownames(var="Sample_id") %>% 
  select(-Subtype) %>% prcomp(.,scale. = TRUE)

var_explained <- pca$sdev^2/sum(pca$sdev^2)

fviz_pca_biplot(pca, axes = c(1, 2),geom.ind = c("point"),geom.var = c("arrow", "text"),
                pointshape = 20,pointsize=4,label ="var",repel = TRUE,col.var = "grey50",
                labelsize=0.5,
                col.ind = df$Subtype)+
  scale_color_manual(values = colorRampPalette(brewer.pal(12,"Paired"))(4))+
  labs(x=paste0("(PC1: ",round(var_explained[1]*100,2),"%)"),
       y=paste0("(PC2: ",round(var_explained[2]*100,2),"%)"),
       title="PCA-Biplot")+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.x = element_text(colour="black",size = 12,margin = margin(t=12)),
        axis.title.y = element_text(colour="black",size = 12,margin = margin(r=12)),
        axis.text=element_text(color="black"),
        plot.title = element_text(size=12,colour = "black",hjust=0.5,face = "bold"),
        legend.title = element_blank(),
        legend.key=element_blank(),   # 图例键为空
        legend.text = element_text(color="black",size=9), # 定义图例文本
        legend.spacing.x=unit(0.1,'cm'), # 定义文本书平距离
        legend.key.width=unit(0.2,'cm'), # 定义图例水平大小
        legend.key.height=unit(0.2,'cm'), # 定义图例垂直大小
        legend.background=element_blank(), # 设置背景为空
        legend.box.background=element_rect(colour="black"), # 图例绘制边框
        legend.position=c(1,0),legend.justification=c(1,0))
