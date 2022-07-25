library(tidyverse)
library(rstatix)
library(ggpubr)
library(ggprism)

df <- read_tsv('F1-a.txt') %>% pivot_longer(-`MUFA-PI / total PI [%]`) %>% 
  group_by(name) %>%
  summarise(value_mean=mean(value),sd=sd(value),se=sd(value)/sqrt(n())) %>% 
  mutate(group=case_when(name=="w/o" ~ "black",name=="TNFα" ~ "grey",
                         name=="I3M" ~ "grey",TRUE ~ "green"))
  
df$name <- factor(df$name,levels=read_tsv('F1-a.txt') %>% 
                    pivot_longer(-`MUFA-PI / total PI [%]`) %>%
                    select(name) %>% distinct() %>% pull())

stat.test <- read_tsv('F1-a.txt') %>% pivot_longer(-`MUFA-PI / total PI [%]`) %>% 
  t_test(data =., value ~ name, ref.group  =  "w/o") %>% 
  mutate(p.adj.signif = replace_na(p.adj.signif,""),across("p.adj.signif",str_replace,"ns","")) %>% 
  select(group1,group2,p.adj,p.adj.signif) %>% 
  left_join(.,df,by=c("group2"="name")) %>% 
  mutate(y.position=value_mean+se+0.3)

theme_niwot <- function(){
  theme_test() +
    theme(axis.title= element_blank(),
          axis.ticks.x=element_blank(),
          panel.grid.major.y = element_line(color = "#DAE1E7"),
          panel.grid.major.x = element_blank(),
          plot.margin = unit(rep(0.2,4),"cm"),
          axis.text = element_text(size = 10, color = "#22292F"),
          axis.text.y = element_text(margin = margin(r = 5)),
          axis.text.x = element_text(margin = margin(t = 5)),
          legend.position = "non")
}


df %>% ggplot(.,aes(name,value_mean))+
  geom_errorbar(aes(ymax = value_mean + se, ymin = value_mean - se),width = 0.2,color = "grey30")+
  geom_col(width=0.5,aes(fill=group),color="grey50")+
  add_pvalue(stat.test,label = "p.adj.signif",label.size=6,
             coord.flip = TRUE, remove.bracket = TRUE)+
  scale_y_continuous(expand=c(0,0),limits = c(0,57)) +
  theme_niwot()+
  scale_fill_brewer(palette="Blues")

