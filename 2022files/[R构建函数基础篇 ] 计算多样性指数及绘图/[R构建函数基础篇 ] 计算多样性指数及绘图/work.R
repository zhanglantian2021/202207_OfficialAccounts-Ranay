library(tidyverse)
library(vegan)
library(rstatix)
library(ggpubr)
library(magrittr)

alpha <- read.delim("otu_taxa_table-2.xls",sep="\t",row.names = 1) %>% 
  t() %>% as.data.frame()

group <- read_tsv("group.xls") %>% set_colnames(c("sample","group"))


alpha_diversity <- function(x,y) {
  Shannon <- diversity(x, index = 'shannon')
  Simpson <- diversity(x, index = 'simpson')  
  observed_species <- specnumber(x)
  Chao1 <- estimateR(x)[2,]
  ACE <- estimateR(x)[4,]
  pielou <- diversity(x,index = "shannon")/log(specnumber(x),exp(1))
  
  result <- data.frame(Shannon,Simpson,observed_species,Chao1,ACE,pielou) %>% 
    rownames_to_column("sample") %>% 
    left_join(.,y,by="sample")
    
  return(result)
  
}

alpha_diversity(alpha,group) %>% write.table(file="alpah.xls",sep="\t",quote = F,row.names = F)


df <- alpha_diversity(alpha,group) %>% select(-sample,-observed_species,-Simpson) %>% 
  pivot_longer(-group)

col <- c("#1F78B4","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#B2DF8A",
         "#A6CEE3","#BA7A70","#9D4E3F","#829BAB")


make_plot <- function(data,x,y,z){
  ggplot(data,aes(x={{x}},y={{y}},fill={{x}}))+
  geom_violin(trim=F)+
    stat_boxplot(geom="errorbar",position=position_dodge(width=0.2),width=0.1)+
    geom_boxplot(position=position_dodge(width =0.8),width=0.1,fill="white")+
    scale_fill_manual(values={{z}})+
    facet_wrap(.~name,scales = "free")+
    theme_bw()+
    theme(panel.spacing.x = unit(0.2,"cm"),
          panel.spacing.y = unit(0.1, "cm"),
          axis.title = element_blank(),
          strip.text.x = element_text(size=12,color="black"),
          axis.text = element_text(color="black"),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.position = "non",
          plot.margin=unit(c(0.3,0.3,0.3,0.3),units=,"cm"))
}


make_plot(df,group,value,col)

