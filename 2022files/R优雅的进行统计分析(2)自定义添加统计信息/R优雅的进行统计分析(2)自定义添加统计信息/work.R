library(tidyverse)
library(rstatix)
library(ggprism)
library(ggpubr)
library(ggsci)

df <- ToothGrowth %>%
  mutate(dose=as.factor(dose)) %>% 
  group_by(dose) %>%
  summarise(value_mean=mean(len),sd=sd(len),se=sd(len)/sqrt(n()))

stat.test <- ToothGrowth %>% t_test(data =., len ~ dose, ref.group  =  "0.5") %>% 
  mutate(p.adj.signif = replace_na(p.adj.signif,""),across("p.adj.signif",str_replace,"ns","")) %>% 
  select(group1,group2,p.adj,p.adj.signif) %>% 
  left_join(.,df,by=c("group2"="dose")) %>% 
  mutate(y.position=value_mean+sd+0.3)

theme_niwot <- function(){
  theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.line = element_line(color = "#3D4852"),
          axis.ticks = element_line(color = "#3D4852"),
          panel.grid.major.y = element_line(color = "#DAE1E7"),
          panel.grid.major.x = element_blank(),
          plot.margin = unit(rep(0.2,4),"cm"),
          axis.text = element_text(size = 12, color = "#22292F"),
          axis.title = element_text(size = 12, hjust = 1),
          axis.title.y = element_text(margin = margin(r = 12)),
          axis.text.y = element_text(margin = margin(r = 5)),
          axis.text.x = element_text(margin = margin(t = 5)),
          legend.position = "non")
}

df %>% ggplot(.,aes(dose,value_mean))+
  geom_errorbar(aes(ymax = value_mean + sd, ymin = value_mean - sd),width = 0.1,color = "grey30")+
  geom_col(width=0.4,aes(fill=dose))+
  add_pvalue(stat.test,label = "p.adj.signif",label.size=6,
             coord.flip = TRUE, remove.bracket = TRUE)+
  scale_y_continuous(expand=c(0,0),limits = c(0,33)) +
  theme_niwot()+
  scale_fill_brewer(palette="Blues")

df %>% ggplot(.,aes(dose,value_mean))+
  geom_errorbar(aes(ymax = value_mean + sd, ymin = value_mean - sd),width = 0.1,color = "grey30")+
  geom_col(width=0.4,aes(fill=dose))+
  stat_pvalue_manual(stat.test %>% slice(1),label = "p.adj.signif",
                     label.size=6,tip.length = c(0.35,0.003),linetype=2)+
  add_pvalue(stat.test %>% slice(2),label = "p.adj.signif",label.size=6,tip.length = c(0.1,0.003))+
  scale_y_continuous(expand=c(0,0),limits = c(0,33)) +
  theme_niwot()+
  scale_fill_brewer(palette="Blues")

stat.test2 <- ToothGrowth %>% mutate(dose=as.factor(dose)) %>% group_by(dose) %>% 
  t_test(len ~ supp) %>%
  adjust_pvalue() %>% add_significance("p.adj") %>% add_xy_position(x="dose") 

stat.test3 <- ToothGrowth %>% 
  t_test(len ~ dose,p.adjust.method = "bonferroni") %>%
  adjust_pvalue() %>% add_significance("p.adj") %>% add_xy_position() 

res.aov <- ToothGrowth %>% mutate(dose=as.factor(dose)) %>% anova_test(len ~ dose)

ToothGrowth %>% mutate(dose=as.factor(dose)) %>% 
  ggplot(aes(dose,len))+
  stat_boxplot(geom = "errorbar",width=0.2,aes(fill = supp),position = position_dodge(1)) +
  geom_boxplot(aes(fill= supp),position = position_dodge(1))+
  stat_pvalue_manual(stat.test2,label = "p.adj.signif",label.size=6,hide.ns = T)+
  stat_pvalue_manual(stat.test3,label = "p.adj.signif",label.size=6,hide.ns = T)+
  labs(subtitle = get_test_label(res.aov, detailed = TRUE))+
  scale_y_continuous(expand=c(0,0),limits = c(0,42))+
  theme_niwot()+
  scale_fill_jco()

stat.test4 <- ToothGrowth %>% mutate(dose=as.factor(dose)) %>% tukey_hsd(len ~ dose) %>% 
  add_xy_position("dose") 

ToothGrowth %>% mutate(dose=as.factor(dose)) %>% 
  ggplot(aes(dose,len))+
  stat_boxplot(geom = "errorbar",width=0.2,aes(fill = supp),position = position_dodge(1)) +
  geom_boxplot(aes(fill= supp),position = position_dodge(1))+
  stat_pvalue_manual(stat.test4,label = "p.adj.signif",label.size=6,hide.ns = T)+
  labs(subtitle = get_test_label(res.aov, detailed = TRUE))+
  scale_y_continuous(expand=c(0,0),limits = c(0,42))+
  theme_niwot()+
  scale_fill_jco()
  

