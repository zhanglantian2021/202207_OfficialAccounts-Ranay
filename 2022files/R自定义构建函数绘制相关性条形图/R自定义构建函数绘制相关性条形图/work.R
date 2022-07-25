library(tidyverse)
library(magrittr)

df <- read.delim("data.xls",row.names = 1,sep="\t")
  
plot_data_prep <- function(data,gene){
  
 plot_data <- data %>% 
    pivot_longer(-gene) %>% 
    pivot_longer(names_to = "name_2", values_to = "value_2",gene) %>%
    group_by(name_2,name) %>% 
    summarise(cor= cor.test(value_2,value,method="spearman")$estimate,
              p.value = cor.test(value_2,value,method="spearman")$p.value) %>% 
    set_colnames(c("gene_1","gene_2","cor","pvalue")) %>% 
    filter(pvalue < 0.05) %>% 
    arrange(desc(abs(cor)))%>% 
    dplyr::slice(1:500) %>% 
   mutate(p_signif=symnum(pvalue,corr = FALSE, na = FALSE,  
                          cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                          symbols = c("***", "**", "*", ".", " "))) 
 
 return(plot_data)

}


gene <- "B2M"
plot_data_prep(df,gene)

make_plot <- function(data,x,y){
  ggplot(data) + 
    geom_col(aes(x={{x}},y={{y}}, fill = {{x}} > 0),
             size = 0.25, color = "white")+
    geom_point(aes(x={{x}},y={{y}},
                   color=ifelse({{x}} > 0,"#BA7A70","#829BAB")),size=4.1)+
    geom_text(aes(x = ifelse({{x}} > 0, -.005, .005),y = {{y}}, 
                  label = {{y}},
                  color=ifelse({{x}} > 0,"#BA7A70","#829BAB"),
                  hjust = ifelse({{x}} > 0, 1, 0)),size = 3.8)+
    geom_vline(xintercept=0,size=1,color="grey40")+
    scale_y_discrete(expand = c(.025,.025))+
    scale_fill_manual(values = c("TRUE" = "#BA7A70","FALSE" = "#829BAB"))+
    scale_color_manual(values = c("#829BAB","#BA7A70"))+
    coord_cartesian(clip = "off") +  
    theme_minimal() + 
    theme(panel.grid = element_blank(),
          plot.background = element_rect(fill="Aliceblue",color="Aliceblue"),
          axis.text.y =  element_blank(),
          axis.title = element_blank(),
          legend.position = "none",
          axis.text.x = element_text(face = "bold", size =rel(1), color = "black"))
}

set.seed(123)

p <- plot_data_prep(df,gene) %>% select(1,2,3,5) %>% sample_frac(.1) %>% 
  arrange(cor)

p$gene_2 <- factor(p$gene_2,levels = p$gene_2)

make_plot(p,cor,gene_2)
