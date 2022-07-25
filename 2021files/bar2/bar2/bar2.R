rm(list=ls())
pacman::p_load(tidyverse,magrittr,reshape,RColorBrewer,ggalluvial)
colors <-c("#E41A1C","#1E90FF","#FF8C00","#4DAF4A","#984EA3",
           "#40E0D0","#FFC0CB","#00BFFF","#FFDEAD","#90EE90",
           "#EE82EE","#00FFFF","#F0A3FF", "#0075DC", 
           "#993F00","#4C005C","#2BCE48","#FFCC99",
           "#808080","#94FFB5","#8F7C00","#9DCC00",
           "#426600","#FF0010","#5EF1F2","#00998F",
           "#740AFF","#990000","#FFFF00")

theme_niwot <- function(){
    theme(text = element_text(family = "Times"),
          axis.line.x = element_line(color="black"), 
          axis.line.y = element_line(color="black"),
          axis.text.x = element_text(family = "Times",size=12,face="plain"),
          axis.text.y = element_text(family = "Times",size=12,face="plain"),
          panel.border = element_blank(),
          axis.title.x = element_text(margin = margin(t = 10),size=13,
                                      family = "Times",color="black"),
          axis.title.y = element_text(margin = margin(r = 10),size=13,
                                      family = "Times",color="black"),
          panel.grid.major.x = element_blank(),                                          
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),  
          plot.margin = unit(c(1, 1, 1, 1), units = ,"cm"),
          legend.text = element_text(size = 12,family ="Times"),
          legend.key = element_blank(),
          panel.background = element_rect(fill = "white"),
          legend.background = element_rect(color = "black", 
          fill = "transparent",size = 2, linetype = "blank"))+
    theme_bw()+theme(legend.title = element_blank())
}
####-----------------------------------------------
computed_persent <- function(path) {
  data <- path %>%
    read.delim(check.names = FALSE, row.names = 1)
  data2 <- data %>%
    mutate(sum = rowSums(.), persent = sum / sum(sum) * 100, sum = NULL,) %>%
    rbind(filter(., persent < 1) %>% colSums()) %>%
    mutate(ID = c(data %>% rownames(), "others"))
  filter(data2[1:(nrow(data2) - 1),], persent > 1) %>%
    rbind(data2[nrow(data2),]) %>%
    set_rownames(seq_len(nrow(.))) %>%
    return()
}

#-------------------------------------------------
path <- "genus.xls" #此处写入的是文件路径
a1 <- computed_persent(path) %>% melt() %>% filter(variable !="persent")

a2 <- "group.txt" %>% read.delim()
a4 <- NULL

for (i in seq_len(nrow(a1))) { 
  a4[i] <- a2[which(a2[, 1] == a1[i, 2]),2] }

a1[,4] <- a4

head(a1)

ggplot(a1,aes(variable,value,alluvium=ID,stratum = ID))+
  geom_alluvium(aes(fill = ID),alpha = .5,width = 0.6) + 
  geom_stratum(aes(fill = ID),width = 0.6)+
  facet_grid(. ~V4,scales = "free",space="free_x")+
  labs(x=NULL,y=NULL)+
  scale_fill_manual(values = colors)+
  scale_y_continuous(expand=c(0,0))+
  theme_niwot()
