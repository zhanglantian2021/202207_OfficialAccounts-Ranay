library(tidyverse)
library(GGally)

unicox <- read_csv("AKT3_mRNA_OS_pancan_unicox.csv") 

p1 <- ggplot(unicox,aes(HR_log, cancer, col=Type))+
  geom_point(aes(size=-log10(p.value)))+
  geom_errorbarh(aes(xmax =upper_95_log, xmin = lower_95_log), height = 0.4)+
  scale_x_continuous(limits= c(-2, 2), breaks= seq(-1, 1, 1))+
  geom_vline(aes(xintercept = 0))+
  xlab('HR(95%CI)') + ylab(' ')+
  theme_bw(base_size = 12)+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_text(color="black"),
        axis.title.y=element_blank(),
        plot.margin = unit(rep(0,4),"cm"),
        legend.title = element_blank(),
        legend.key=element_blank(),   # 图例键为空
        legend.text = element_text(color="black",size=9), # 定义图例文本
        legend.spacing.x=unit(0.1,'cm'), # 定义文本书平距离
        legend.spacing.y=unit(0.1,'cm'), 
        legend.key.width=unit(0.2,'cm'), # 定义图例水平大小
        legend.key.height=unit(0.2,'cm'), # 定义图例垂直大小
        legend.background=element_blank(), # 设置背景为空
        legend.box.background=element_rect(colour="black"), # 图例绘制边框
        legend.position=c(1,0),legend.justification=c(1,0))+
  scale_color_manual(values = c("gray", "steelblue", "red"))

p2 <- unicox %>% select(cancer) %>% distinct() %>% mutate(group="A") %>% 
  ggplot(aes(group,cancer))+
  geom_text(aes(group,cancer,label=cancer ),size=3,color="black") +
  geom_stripped_rows()+
  theme(panel.grid.major = element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        axis.title.y=element_blank(),
        plot.margin = unit(rep(0,4),"cm"))+
  labs(x="Gene")+
  scale_x_discrete(position = "top")
  
p3 <- unicox %>% select(6,7,8) %>% mutate(across(where(is.numeric),~round(.,2))) %>% 
  mutate(`HR(95%Cl)`=paste(HR_log,"[",lower_95_log," to ",upper_95_log,"]",sep="")) %>% select(4) %>%
  mutate(group="HR(95%Cl)") %>% 
  ggplot(aes(group,`HR(95%Cl)`))+
  geom_text(aes(group,`HR(95%Cl)`,label=`HR(95%Cl)`),size=3,color="black") +
  geom_stripped_rows()+
  theme(panel.grid.major = element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        axis.title.y=element_blank(),
        plot.margin = unit(rep(0,4),"cm"))+
  labs(x="HR(95%Cl)")+
  scale_x_discrete(position = "top")



p4 <- unicox %>% select(p.value) %>% 
  mutate(across(where(is.numeric),~round(.,2))) %>% 
  mutate(group="A",p.value=as.character(p.value)) %>% 
  ggplot(aes(group,p.value))+
  geom_text(aes(group,p.value,label=p.value),size=3,color="black") +
  geom_stripped_rows()+
  theme(panel.grid.major = element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        axis.title.y=element_blank(),
        plot.margin = unit(rep(0,4),"cm"))+
  labs(x="p.value")+
  scale_x_discrete(position = "top")



library(patchwork)

(p2+p3+p4+p1)+plot_layout(widths = c(.12,0.28,.15,.8))

