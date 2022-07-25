library(tidyverse)
library(rstatix)
library(ggtext)

df <- ToothGrowth %>%
  mutate(dose=as.factor(dose)) %>% 
  group_by(dose) %>%
  summarise(value_mean=mean(len),sd=sd(len),se=sd(len)/sqrt(n())) %>% 
  left_join(.,ToothGrowth %>% t_test(data =., len ~ dose, ref.group  =  "0.5") %>% 
              adjust_pvalue(method = "bonferroni") %>% 
              select(group2,p.adj.signif),by=c("dose"="group2")) %>% 
  mutate(p.adj.signif = replace_na(p.adj.signif,""),across("p.adj.signif",str_replace,"ns","")) %>% 
  ungroup()

ggplot(df,aes(dose,value_mean,fill=dose))+
  geom_errorbar(aes(ymax = value_mean + sd, ymin = value_mean - sd),width = 0.1,color = "grey30")+
  geom_col(width=0.4)+
  geom_text(aes(label=p.adj.signif, y = value_mean + sd +1.5), size = 3, color = "white",
            show.legend = FALSE)+
  geom_text(aes(label=p.adj.signif, y = value_mean + sd + 0.2),size=5, color = "black",
            show.legend = FALSE)+
  scale_y_continuous(expand=c(0,0)) +
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
        legend.position = "non")+
  scale_fill_brewer(palette="Blues")
