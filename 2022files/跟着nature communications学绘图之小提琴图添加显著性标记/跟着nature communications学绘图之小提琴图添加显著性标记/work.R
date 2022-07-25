library(tidyverse)
library(magrittr)

col=c("#1F78B4","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#B2DF8A","#A6CEE3")

df <- read_tsv("data.xls")
df$Compartments <- factor(df$Compartments,levels = c("BS","RS","RE","VE","SE","LE","P"))

text <- df %>% group_by(Compartments) %>% summarise(max(`Shannon index`)) %>% 
  set_colnames(c("group","value")) %>% 
  mutate(value=value+0.3,p=c("a","b","c","g","f","e","d"))

df %>% set_colnames(c("sample","value","group")) %>% 
  ggplot(aes(group,value,fill=group))+
  geom_violin(position = position_dodge(width = 0.1), scale = 'width')+ 
  stat_boxplot(geom="errorbar",position = position_dodge(width = 0.1),width=0.1)+
  geom_boxplot(alpha=1,outlier.size=0, size=0.3, width=0.3,fill="white")+
  geom_text(data=text,aes(label=p,y=value),size=5,
            color=c("#1F78B4","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#B2DF8A","#A6CEE3"))+
  labs(x=NULL, y="Shannon index")+
  scale_fill_manual(values = col)+
  theme_test() + 
  theme(axis.text.x = element_text(size = 8,color="black"),
        axis.text.y = element_text(size = 8,color="black"),
        axis.title.y= element_text(size=12,color="black"),
        axis.title.x = element_text(size = 12,color="black"),
        legend.title=element_blank(),
        legend.key=element_blank(), 
        legend.box.background = element_blank(),
        legend.text = element_text(color="black",size=8),
        legend.spacing.y = unit(0.3,"cm"),
        legend.spacing.x=unit(0.1,'cm'), 
        legend.key.width=unit(0.3,'cm'), 
        legend.key.height=unit(0.3,'cm'),
        legend.background=element_blank(), 
        legend.position=c(0.99,0.9999),legend.justification=c(1,1))
