library(tidyverse)
library(scales)
library(ggh4x)
library(patchwork)
library(magrittr)

computed_persent <- function(path) {
  data <- path %>%
    read.delim(check.names = F,sep="\t",row.names = 1) %>% 
    t() %>% as.data.frame()
  data2 <- data %>%
    mutate(sum = rowSums(.), persent = sum / sum(sum) * 100, 
           sum = NULL,) %>%
    rbind(filter(., persent < 0.1) %>% colSums()) %>%
    mutate(Taxa = c(data %>% rownames(), "others"))
  filter(data2[1:(nrow(data2) - 1),], persent > 0.1) %>%
    rbind(data2[nrow(data2),]) %>%
    select(ncol(.), 1:(ncol(.) - 2)) %>%
    set_rownames(seq_len(nrow(.))) %>%
    return()
}

otu_taxa <- computed_persent("otu.xls") %>% 
  pivot_longer(cols = !Taxa,names_to = "Samples",
               values_to = "number") %>% arrange(desc(number))

meta_taxa <- read.delim("taxa.xls",check.names = F,sep="\t") %>% 
  inner_join(.,otu_taxa,,by="Samples")

meta_taxa$Taxa <- factor(meta_taxa$Taxa,levels = unique(meta_taxa$Taxa))

palette <-c("#00545b","#ff856d","#640025","#3ddda5","#cdffaa","#150e00","#bae278",
            "#007a98","#ffe093","#00533f","#90f0ff","#6d3c00","#004f17")

p2 <- ggplot(meta_taxa,aes(Samples,number,fill=Taxa))+
  geom_col(position="stack") +
  facet_nested(.~Type+Trial+Day,drop=T,
               scale="free",space="free",switch="x")+
  scale_fill_manual(values=palette)+
  labs(x=NULL, y="Percent Phyla Abundance")+
  scale_y_continuous(expand = c(0,0),labels=scales::percent)+
  theme(strip.background = element_rect(fill="white",color="black"),
        panel.spacing = unit(0,"lines"),
        strip.text.x = element_text(size=12,color="black"),
        axis.text.y=element_text(size=12),
        axis.title.y = element_text(size=12,color="black"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  labs(fill="Phylum")

g <- ggplot_gtable(ggplot_build(p2))

strips <- which(grepl('strip-', g$layout$name))

pal <- c("#E64B35FF","#4DBBD5FF","#00A087FF","#3C5488FF","#F39B7FFF","#8491B4FF","#91D1C2FF",
         "#FF0000","#4DBBD5FF","#00A087FF","#3C5488FF","#F39B7FFF","#8491B4FF","#91D1C2FF",
         "#F8AFA8","#4DBBD5FF","#B09C85FF","#3C5488FF","#F39B7FFF","#B09C85FF","#91D1C2FF",
         "#D3DDDC","#00A087FF","#E6A0C4","#3C5488FF")

for (i in seq_along(strips)) {
  k <- which(grepl('rect', g$grobs[[strips[i]]]$grobs[[1]]$childrenOrder))
  l <- which(grepl('titleGrob', g$grobs[[strips[i]]]$grobs[[1]]$childrenOrder))
  g$grobs[[strips[i]]]$grobs[[1]]$children[[k]]$gp$fill <- pal[i]
  # g$grobs[[strips[i]]]$grobs[[1]]$children[[l]]$children[[1]]$gp$col <- pal[i] #设置字体颜色
}

plot(g)

#-----------------------------------------------------------------------------------------------
library(aplot)
library(tidyverse)
library(scales)

computed_persent <- function(path) {
  data <- path %>%
    read.delim(check.names = F,sep="\t",row.names = 1) %>% 
    t() %>% as.data.frame()
  data2 <- data %>%
    mutate(sum = rowSums(.), persent = sum / sum(sum) * 100, 
           sum = NULL,) %>%
    rbind(filter(., persent < 0.1) %>% colSums()) %>%
    mutate(Taxa = c(data %>% rownames(), "others"))
  filter(data2[1:(nrow(data2) - 1),], persent > 0.1) %>%
    rbind(data2[nrow(data2),]) %>%
    select(ncol(.), 1:(ncol(.) - 2)) %>%
    set_rownames(seq_len(nrow(.))) %>%
    return()
}

otu_taxa <- computed_persent("otu.xls") %>% 
  pivot_longer(cols = !Taxa,names_to = "Samples",
               values_to = "number") %>% arrange(desc(number))

meta_taxa <- read.delim("taxa.xls",check.names = F,sep="\t") %>%
  inner_join(.,otu_taxa,,by="Samples") %>%
  mutate(Taxa=factor(Taxa,levels = unique(Taxa)))


palette <- c("#709AE1FF","#8A9197FF","#D2AF81FF","#FD7446FF",
  "#D5E4A2FF","#197EC0FF","#F05C3BFF","#46732EFF",
"#71D0F5FF","#370335FF","#075149FF","#C80813FF","#91331FFF","#1A9993FF","#FD8CC1FF")

colors1 <- c("#3B9AB2","#FF0000","#00A08A","#F2AD00","#F98400","#5BBCD6")
colors2 <- c("#E64B35FF","#4DBBD5FF","#00A087FF")
colors3 <- c("#0073C2FF","#EFC000FF","#868686FF")

theme_niwot <- function(){
  theme(
  legend.key=element_blank(),   # 图例键为空
  legend.text = element_text(color="black",size=10), # 定义图例文本
  legend.spacing.x=unit(0.1,'cm'), # 定义文本书平距离
  legend.key.width=unit(0.5,'cm'), # 定义图例水平大小
  legend.key.height=unit(0.5,'cm'), # 定义图例垂直大小
  legend.background=element_blank())
}


bar <- ggplot(meta_taxa,aes(Samples,number,fill=Taxa))+
  geom_col(position="stack") +
  scale_fill_manual(values=palette)+
  labs(y="Percent Phyla Abundance")+
  scale_y_continuous(expand = c(0,0),labels=scales::percent)+
  scale_x_discrete(expand=c(0,0)) +
  theme(axis.text.x=element_blank(),
               axis.ticks.x=element_blank(),
               axis.title.x=element_blank(),
               axis.text.y=element_text(color="black",size=10),
               axis.title.y=element_text(color="black",size=11),
               panel.grid.major=element_blank(), 
               panel.grid.minor=element_blank(), 
               panel.background = element_blank())+
  theme_niwot()
  
group1 <- meta_taxa %>% select(1,2) %>% mutate(group="Type") %>% 
  ggplot(aes(Samples,group,fill=Type))+
  geom_tile()+
  scale_y_discrete(expand = c(0,0),position="right")+
  scale_x_discrete(expand=c(0,0)) +
  scale_fill_manual(values=colors3)+
  theme_void()+
  theme(axis.text.y=element_text(color="black",size=10))+
  theme_niwot()

group2 <- meta_taxa %>% select(1,3) %>% mutate(group="Trial") %>% 
  as_tibble() %>% 
  mutate(Trial=as.character(Trial)) %>% 
  ggplot(aes(Samples,group,fill=Trial))+
  geom_tile()+
  scale_fill_manual(values=colors2)+
  scale_y_discrete(position="right")+
  scale_x_discrete(expand=c(0,0))+
  theme_void()+
  theme(axis.text.y=element_text(color="black",size=10))+
  theme_niwot()

group3 <- meta_taxa %>% select(1,4) %>% mutate(group="Day") %>% 
  as_tibble() %>% 
  mutate(Day=as.character(Day)) %>% 
  ggplot(aes(Samples,group,fill=Day))+
  geom_tile()+
  scale_fill_manual(values=colors1)+
  scale_y_discrete(expand = c(0,0),position="right")+
  scale_x_discrete(expand=c(0,0))+
  theme_void()+
  theme(axis.text.y=element_text(color="black",size=10))+
  theme_niwot()


bar %>% insert_bottom(group3,height = 0.05) %>% 
  insert_bottom(group2,height = 0.05) %>% 
  insert_bottom(group1,height = 0.05)

