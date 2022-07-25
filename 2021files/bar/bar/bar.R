rm(list=ls())
pacman::p_load(tidyverse,reshape,RColorBrewer)
colors <-c("#E41A1C","#1E90FF","#FF8C00","#4DAF4A","#984EA3",
           "#40E0D0","#FFC0CB","#00BFFF","#FFDEAD","#90EE90",
           "#EE82EE","#00FFFF","#F0A3FF", "#0075DC", 
           "#993F00","#4C005C","#2BCE48","#FFCC99",
           "#808080","#94FFB5","#8F7C00","#9DCC00",
           "#426600","#FF0010","#5EF1F2","#00998F",
           "#740AFF","#990000","#FFFF00")

a1 <- read.delim("data.xls",header = T,row.names = 1,
                 check.names = F,sep="\t") %>% 
  mutate(sum= rowSums(.),ID=row.names(.),
         persent = sum/sum(sum)*100,sum=NULL) %>%
  filter(persent >=1) %>% 
  melt() %>% filter(variable !="persent")

a2 <- "group.xls" %>% read.delim()
a4 <- NULL

for (i in seq_len(nrow(a1))) { 
  a4[i] <- a2[which(a2[, 1] == a1[i, 2]),2] }

a1[,4] <- a4

ggplot(a1,aes(variable,value,fill=ID))+
  geom_bar(stat="identity",position = "fill")+
  facet_grid(.~ V4,scales = "free",space="free_x")+
  labs(x="",y="Proportions")+
  scale_fill_manual(values =colors)+labs(fill="")+
  theme(legend.title=element_blank())+
  scale_y_continuous(expand=c(0,0))+theme_bw()


