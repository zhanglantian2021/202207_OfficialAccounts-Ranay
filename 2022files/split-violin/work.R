library(tidyverse)
library(introdataviz)
library(ggpubr)
library(ggsci)
library(ggprism)
library(scales)
library(patchwork)

dd2 <- read_tsv("E:/academic_resources/202207_OfficialAccounts-Ranay/2022files/split-violin/F6.txt") %>% pivot_longer(- lncRNA_signature)
colors = c("#5a72b5","#ea5a49")

a <- ggplot(dd2,aes(x = name, y = value,fill= lncRNA_signature))+
  geom_split_violin(trim =F,color = NA,adjust = 1.5)+
  guides(fill=guide_legend(title="group"))+
  scale_fill_manual(values = colors)+
  stat_summary(fun.data = "mean_sd",position=position_dodge(0.15),geom = "errorbar",width = .1) +
  stat_summary(fun = "mean", geom = "point", position=position_dodge(0.15),show.legend = F)+
  stat_compare_means(aes(group = lncRNA_signature), label = "p.signif",label.y=12, method="t.test")+
  # label = p.signif & p.format
  labs(x=NULL,y=NULL)+
  theme(axis.title = element_blank(),
      axis.text.x=element_text(angle =45,hjust =1,vjust =1,color="black",size = 10,margin = margin(b =2)),
      axis.text.y=element_text(color="black",size = 10,margin = margin(r =1)),
      panel.background = element_rect(fill = NA,color = NA),
      panel.grid.minor= element_line(size=0.2,color="#e5e5e5"),
      panel.grid.major = element_line(size=0.2,color="#e5e5e5"),
      panel.border = element_rect(fill=NA,color="black",size=1,linetype="solid"),
      legend.key=element_blank(),
      legend.title = element_text(color="black",size=10),
      legend.text = element_text(color="black",size=8),
      legend.spacing.x=unit(0.1,'cm'),
      legend.key.width=unit(0.5,'cm'),
      legend.key.height=unit(0.5,'cm'),
      legend.box.background=element_rect(colour = "black"), 
      legend.position = c(1,0), legend.justification = c(1,0),
      legend.background=element_blank())


dat <- read_tsv("f6C.txt")
dat$lncRNA_signature <- factor(dat$lncRNA_signature,levels = c("low-risk","high-risk"))
groups <- unique(dat$group)
plots = list()

for(group_ in groups) {
  plots[[group_]] <-ggplot(dat %>%filter(group ==group_),
                           aes(x=lncRNA_signature,y=Intratumor_CD20),alpha=0.5) + 
    stat_boxplot(geom="errorbar",position=position_dodge(width=0.2),width=0.1)+
    geom_boxplot(position=position_dodge(width =0.2),width=0.4)+
    geom_jitter(aes(fill=lncRNA_signature,group=group,size=1,alpha=0.8),pch=21)+
    scale_size_continuous(range=c(1,3))+
    stat_compare_means(aes(group = lncRNA_signature), label = "p.format",method="t.test")+
    scale_fill_manual(values = c("#000091","#ea5a49"))+
    scale_x_discrete(guide = "prism_bracket")+
    scale_y_continuous(labels = label_number(scale = 1e-3))+
    labs(x=NULL,y="Stromal CD20+ B cell count\n(10e3/mm2)")+
    theme_prism(base_line_size =0.5)+
    theme(plot.margin=unit(c(0,0,0.5,0),units=,"cm"),
          axis.line = element_line(color = "black",size = 0.4),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.2,color = "#e5e5e5"),
          axis.text.y = element_text(color="black",size=8),
          axis.text.x = element_text(margin = margin(t = -5),color="black",size=8),
          axis.title.y=element_text(color="black",size=10),
          legend.position = "none",
          panel.spacing = unit(0,"lines"))+
    coord_cartesian()
  
  print(plots[[group_]])
}

b <- patchwork::wrap_plots(plots,nrow=1)

(a/b)+ plot_layout(height=c(1.2,0.9))
