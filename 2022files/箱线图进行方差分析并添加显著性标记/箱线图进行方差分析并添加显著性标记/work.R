library(tidyverse)
library(vegan)
library(magrittr)
library(multcompView)

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


col <- c("#1F78B4","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#B2DF8A",
         "#A6CEE3","#BA7A70","#9D4E3F","#829BAB")


df <- alpha_diversity(alpha,group) %>% select(-sample,-observed_species,-Simpson) %>% 
  pivot_longer(-group)

p <- split(df,list(df$name))
aov_data <- data.frame()

str(p)

for(i in 1:4) {
  anova <- aov(value ~ group,data=p[i] %>% as.data.frame() %>% 
                 set_colnames(c("group","name","value")))
  
  Tukey <- TukeyHSD(anova)
  cld <- multcompLetters4(anova,Tukey)
  
  dt <- p[i] %>% as.data.frame() %>% 
    set_colnames(c("group","name","value")) %>% 
    group_by(group,name) %>%
    summarise(value_mean=mean(value),sd=sd(value)) %>%
    ungroup() %>% 
    arrange(desc(value_mean)) %>% 
    as.data.frame()
  
  
  cld <- as.data.frame.list(cld$`group`)
  dt$Tukey <- cld$Letters
  
  aov_data  <- rbind(aov_data,dt)
}


df2 <- df %>% arrange(name) %>% left_join(.,aov_data,by=c("group","name"))

text <- df2 %>% group_by(group,name) %>% summarise(max(value)) %>% arrange(name) %>% ungroup() %>% 
  set_colnames(c("group","name","value")) %>% 
  left_join(.,df2 %>% select(1,2,6),by=c("group","name")) %>% distinct() %>% 
  mutate(value=case_when(name =="ACE" ~ value+90,
                         name =="Chao1" ~ value+90,
                         name =="pielou" ~ value +0.008,
                         name =="Shannon" ~ value+0.065))

  
make_plot <- function(data,x,y,z){
  ggplot(data,aes(x={{x}},y={{y}},fill={{x}}))+
    stat_boxplot(geom="errorbar",position=position_dodge(width=0.2),width=0.2)+
    geom_boxplot(position=position_dodge(width =0.2),width=0.5,outlier.shape = NA)+
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


make_plot(df,group,value,col)+
  geom_text(data=text,aes(label=Tukey,y=value))


