library(tidyverse)
library(MetBrewer)
library(ggsignif)
library(patchwork)
library(survival)
library(survminer)

theme_niwot <- function(){
  theme(panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(margin = margin(r =3),size = 11,face="bold",color="black"),
        axis.text = element_text(color="black",size=11),
        panel.border = element_rect(linetype = "solid",colour = "black", fill = "NA", size = 1),
        legend.position = "non")
}

p1 <- read_tsv("F6-b.txt")%>% select(1,3) %>% 
  left_join(.,read_tsv("F6-a.txt"),by=c("sampleID"="Patient_ID")) %>% 
  select(Subtype,`Non-silent per Mb`) %>% mutate(Subtype=as.character(Subtype)) %>% 
  ggplot(aes(Subtype,`Non-silent per Mb`,fill=Subtype))+
  geom_boxplot(outlier.shape = NA,linetype = "dashed",width=0.5,color="black")+
  stat_boxplot(aes(ymin = ..lower.., ymax = ..upper..),outlier.shape = NA,width=0.5) +
  stat_boxplot(geom = "errorbar", aes(ymin = ..ymax..),width=0.2) +
  stat_boxplot(geom = "errorbar", aes(ymax = ..ymin..),width=0.2) +
  stat_summary(geom = "crossbar", fun = "mean",linetype = "dotdash", width = 0.5,color="black")+
  ylim(0,20)+
  scale_fill_manual(values =c("#6A3D9A","#1F78B4" ,"#33A02C"))+
  labs(x=NULL)+  theme_niwot()

p2 <- read_tsv("F6-b.txt") %>% mutate(Subtype=as.character(Subtype)) %>% 
  ggplot(aes(Subtype,HRD,fill=Subtype))+
  geom_boxplot(outlier.shape = NA,linetype = "dashed",width=0.5,color="black")+
  stat_boxplot(aes(ymin = ..lower.., ymax = ..upper..),outlier.shape = NA,width=0.5) +
  stat_boxplot(geom = "errorbar", aes(ymin = ..ymax..),width=0.2) +
  stat_boxplot(geom = "errorbar", aes(ymax = ..ymin..),width=0.2) +
  stat_summary(geom = "crossbar", fun = "mean",linetype = "dotdash", width = 0.5,color="black")+
  geom_signif(comparisons = list(c("1","3"),c("2","3")),
              map_signif_level=T,vjust=0.5,color="black",
              textsize=6,test=wilcox.test,step_increase=0.1)+
  scale_fill_manual(values =c("#6A3D9A","#1F78B4" ,"#33A02C"))+
  labs(x=NULL)+theme_niwot()

df <- read_tsv("F6-b.txt")%>% select(1,3) %>% 
  left_join(.,read_tsv("F6-e.txt"),by=c("sampleID"="Sample")) %>% 
  left_join(.,read_tsv("F6e-CYT.txt"),by=c("sampleID"="Sample")) %>% 
  left_join(.,read_tsv("F6e-MHC.txt") %>% 
              dplyr::rename("MHC Score"="Score"),by=c("sampleID"="Sample")) %>% 
  select(-Project.x,-Project.y,-Project,-sampleID) %>% 
  mutate(Subtype=as.factor(Subtype)) %>% 
  pivot_longer(-Subtype)

df$name <- factor(df$name,levels = c("Immune Score","CYT-Score","MHC Score"))

p3 <- ggplot(df,aes(x=value,color=Subtype))+
  facet_wrap(.~name,scales="free_x")+
  stat_ecdf()+
  labs(y="cumulative distribution funcition (CDF)",x=NULL)+
  scale_color_manual(values =c("#6A3D9A","#1F78B4" ,"#33A02C"))+
  theme(panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(margin = margin(r =5),size = 11,color="black",face="bold"),
        axis.text = element_text(color="black",size=11),
        panel.border = element_rect(linetype = "solid",colour = "black", fill = "NA",size=1),
        strip.text.x = element_text(colour ="black",size=11),
        strip.background = element_rect(fill="#00A08A"),
        panel.spacing.x = unit(0.1, "cm"),
        legend.title = element_blank(),
        legend.key=element_blank(),  
        legend.text = element_text(color="black",size=10), 
        legend.spacing.x=unit(0.1,'cm'), 
        legend.background=element_blank(),
        legend.position = c(0,1), legend.justification = c(0,1))

df <- read_tsv("F5-f.txt") %>% select(SurvivalTime,vital_status,Subtype) %>%
  mutate(Subtype=as.factor(Subtype),
         vital_status=as.logical(case_when(vital_status =="alive" ~ "TRUE",vital_status =="dead" ~ "FALSE")))

fit <- survfit(Surv(SurvivalTime,vital_status) ~ Subtype, data = df)

p4 <- ggsurvplot(fit,risk.table=F,risk.table.col="strata",conf.int.style = "step",
           pval = "Log-rank P = 0.02",
           pval.coord=c(4000,0.99),
           size=0.8,
           font.legend =8,
           palette=c("#6A3D9A","#1F78B4" ,"#33A02C"),
           ggtheme=theme_bw()+
           theme(panel.border = element_rect(fill=NA,color="black", size=1, linetype="solid"),
                 axis.line = element_line(colour = "black"),
                 axis.text = element_text(hjust = 0.5, size = 11,colour = "black"),
                 axis.title=element_text(face="bold",size=11,colour = 'black',margin = margin(r =3)), 
                 legend.title = element_blank(),
                 legend.key=element_blank(),  
                 legend.text = element_text(color="black",size=10), 
                 legend.spacing.x=unit(0.1,'cm'), 
                 legend.background=element_blank(),
                 legend.position = c(0,1),legend.justification = c(0,1)),
           legend.labs=c(paste0("C1","(",fit$n[1],")"),
                         paste0("C2","(",fit$n[2],")"),
                         paste0("C3","(",fit$n[3],")")))

(p1+p2+p4$plot+plot_layout(ncol = 3, width = c(1, 1,2)))/p3+
  plot_annotation(tag_levels = 'A')
  




