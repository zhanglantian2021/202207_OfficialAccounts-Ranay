library(tidyverse)
library(ggrepel)

# ctrl+shift+M 即可打出 管道符号 %>% 
df <- read_tsv("genes.counts..DESeq2.xls") %>%
  mutate(gene_type=if_else(
    padj > 0.05,"ns",if_else(
      abs(log2FoldChange) < 1,"ns",
      if_else(log2FoldChange >=1,"up","down"))),
    FC=2**log2FoldChange)
  
df %>% group_by(gene_type) %>% 
  summarise(count=n())

cols <- c("up [1395]"="#FF0000","down [1463]"="#00A08A","ns [28078]"="grey")

ggplot(df %>% mutate(gene_type=case_when(
  gene_type =="up" ~ "up [1395]",
  gene_type =="down" ~ "down [1463]",
  gene_type =="ns" ~ "ns [28078]")),aes(log2FoldChange,-log10(pvalue))) + 
  geom_point(aes(color=gene_type,size=abs(log2FoldChange)))+
  geom_hline(yintercept = -log10(0.05),linetype ="dashed") + 
  geom_vline(xintercept = c(-1,1),linetype = "dashed")+
  geom_label_repel(data=df %>%
                     filter(abs(log2FoldChange) >3,padj < 1e-100,
                            id %in% c("TF31923","TF00970","TF26429")),
                   aes(label=id))+
  scale_color_manual(values = cols) + 
  scale_size(range=c(0.1,3))+
  guides(size="none")+
  scale_x_continuous(breaks=c(seq(-10,10,2)),limits = c(-10,10))+
  theme(panel.grid.major=element_blank(), # 移除主网格线
        panel.grid.minor=element_blank(), # 移除次网格线
        panel.background = element_blank(), # 设置背景为空
        axis.title.x=element_text(color="black",size=11,margin = margin(t = 5)),
        axis.title.y=element_text(color="black",size=11,margin = margin(t = 5)),
        axis.text.x=element_text(color="black",margin = margin(t = 5)), # 设置X轴文本颜色
        axis.text.y=element_text(color="black",margin = margin(r = 5)), # 设置y轴文本颜色
        panel.border = element_rect(linetype = "solid",fill = NA), # 定义边框线条类型
        plot.margin=unit(c(0.5,0.5,0.5,0.5),units=,"cm"),
        legend.title = element_blank(),
        legend.key=element_blank(),   # 图例键为空
        legend.text = element_text(color="black",size=10), # 定义图例文本
        legend.spacing.x=unit(0,'cm'), # 定义文本书平距离
        legend.background=element_blank(), # 设置背景为空
        legend.box.background=element_rect(colour = "black"), # 图例绘制边框
        legend.box.margin = margin(1,1,1,1),
        legend.position = c(0,1),legend.justification = c(0,1))
  
  
  
  
