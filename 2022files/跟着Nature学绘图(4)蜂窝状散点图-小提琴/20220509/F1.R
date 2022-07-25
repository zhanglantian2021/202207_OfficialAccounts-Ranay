library(tidyverse)
library(openxlsx)
library(ggbeeswarm)
library(RColorBrewer)
library(MetBrewer)
library(ComplexHeatmap)
library(patchwork)
library(ggplotify)


c <- read_tsv("F1_C.xls") %>% 
  mutate(score_log = log10(score + min(score[score > 0]) / 2)) %>% 
  ggplot(.,aes(x = score_type, y = score_log)) +
  geom_violin(scale = "width", width = 0.8) +
  geom_quasirandom(aes(colour = fuso_load),size = 0.5,alpha = 0.5) +
  coord_flip() +
  theme_classic(7) +
  scale_colour_manual(values = c("#bd3106","#5b7314","#454b87","#d9700e","#89a6bb","#eebe04"),
                      labels = c("No", "Low", "High")) +
  labs(colour = "Fusobacterium\nload",
       y = "_Fusobacterium_ abundance (log<sub>10</sub> score)",
       x = NULL,
       title = "Cancer tissue sample") +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x=element_text(size=8,color="black"),
        plot.title = element_text(size =10,vjust=0.5,hjust=0.5,color="black"),
        axis.title.x = element_markdown(size=10),
        plot.margin = margin(l=5,t=5,b=5,r=10))

d <- read_tsv('F1_D.xls',col_types = cols()) %>% 
      count(cms, score_type, fuso_load) %>% 
      group_by(cms, score_type) %>% 
  mutate(prop = n / sum(n)) %>% 
  ggplot(.,aes(x = cms,y = prop,fill = fct_rev(fuso_load))) +
  geom_col() + theme_classic(7) +
  scale_fill_manual(values = c("#bd3106","#5b7314","#454b87","#d9700e","#89a6bb","#eebe04"),
                        breaks = c("c0_no", "c1_low", "c2_high"),
                        labels = c("No", "Low", "High")) +
      theme(legend.position = "right",
            legend.box.margin=margin(0, 0, 0, -10),
            legend.key.height = unit(10, "pt"),
            legend.key.width = unit(10, "pt"),
            legend.title = element_markdown()) +
      labs(y = "Proportion",x = NULL,fill = "_Fusobacterium_<br>load") +
      guides(fill = guide_legend(title.position = "top"))


a2 <- read_tsv("F1-A-2.xls") %>%
  ggplot(.,aes(Health.status,log10(value),
           colour=Health.status)) + 
  geom_jitter(width = 0.3, size =2, stroke = 0) +
  geom_violin(colour = "grey", fill = NA) +
  theme_bw() +  
  facet_wrap(~ Genus, ncol = 2)+
  scale_y_continuous(breaks = 0:6) +
  scale_color_manual(values = c("orange", "steelblue"))+
  coord_flip()+
  theme(legend.position = "none", 
        axis.title.x = element_markdown(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.background = element_blank(),
        panel.spacing.y = unit(0, "mm"),
        strip.text.x = element_text(face = "italic",size = 10,margin = margin(0,0,0, 0, "pt"))) +
  labs(x = NULL)


get_reverted_fig1_cor_hc <- function(mtx) {
  hc <- hclust(
    d = as.dist(1 - cor(mtx, method = "pearson")),
    "complete")
  sv <- svd(mtx)$v[, 1]
  dend <- reorder(as.dendrogram(hc), wts = sv)
  as.hclust(dend)
}

plot_fig1_heatmap <- function(mtx, ha, boi) {
  Heatmap(mtx,name = "Abundance",
          col = colorRamp2(c(0, 6), c("#000033","#66CCFF")),
          show_column_names = FALSE,row_split = 2,column_split = 2, 
          row_names_gp = gpar(fontsize = 6,fontface = "italic",
                              col = if_else(rownames(mtx) %in% boi,"red", "black")),
          heatmap_legend_param = list(
            title_gp = gpar(fontsize = 7),
            labels_gp = gpar(fontsize = 6),
            legend_height = unit(15, "mm"), 
            direction = "vertical",
            grid_width = unit(8, "pt"),
            grid_height = unit(10, "pt")
          ),
          row_title = NULL,
          column_title = NULL,
          cluster_rows = get_reverted_fig1_cor_hc(t(mtx)),
          cluster_columns = get_reverted_fig1_cor_hc(mtx),
          column_dend_height = unit(5, "mm"),
          row_dend_width = unit(4, "mm"),
          top_annotation = ha, 
          height = unit(nrow(mtx) * unit(12, "pt")),
          width = unit(ncol(mtx) * unit(2, "pt")),
          raster_quality = 5, use_raster = TRUE
  )
}


fig_1a_mtx <- read_tsv("f1-a1-1.txt") %>% 
  mutate(across(phylum, str_replace_all, "_", " ")) %>%
      column_to_rownames("phylum") %>% 
      as.matrix()
  

fig_1a_annotation <- read_tsv("f1-A1.txt") %>% select(samples, Health.status) %>% 
      filter(samples %in% colnames(fig_1a_mtx)) %>% 
      as.data.frame() %>% 
      select(samples, `Patient status` = Health.status) %>% 
      mutate(across(`Patient status`, recode,
                    "no_cancer" = "Healthy",
                    "cancer" = "Cancer")) %>% 
      deframe()
  

fig_1a_ha <- HeatmapAnnotation("Patient status" = fig_1a_annotation[colnames(fig_1a_mtx)],
                               simple_anno_size = unit(2, "mm"),
      annotation_name_gp = gpar(fontsize =9,lineheight = 0.5),
      annotation_legend_param = list(
        title_gp = gpar(fontsize = 7),
        labels_gp = gpar(fontsize =6)
      ),
      col = list("Patient status" = c(Healthy = "steelblue",
                                      Cancer = "orange")))
a1 <- plot_fig1_heatmap(fig_1a_mtx,
                      fig_1a_ha,
                      c("Streptococcus",
                        "Pseudomonas",
                        "Gemella",
                        "Prevotellaceae Ga6A1 group",
                        "Prevotella 7",
                        "Peptostreptococcus",
                        "Parvimonas",
                        "Porphyromonas",
                        "Fusobacterium",
                        "Prevotella"))

p2 <- (c|d)
((p2/(a1 %>% as.ggplot() %>% ggdraw())+
    plot_layout(nrow=2,heights = c(.3,1.2)))|a2)+
  plot_layout(ncol=2,widths  = c(1.2,1))
  

  
