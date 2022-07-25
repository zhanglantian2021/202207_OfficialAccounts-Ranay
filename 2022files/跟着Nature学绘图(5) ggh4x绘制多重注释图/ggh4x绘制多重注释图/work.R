library(tidyverse)
library(ggsignif)
library(ggprism)
library(ggsci)
library(ggh4x)
library(patchwork)

a <- read_tsv("F3-a.xls") %>% 
  select(-1) %>% pivot_longer(-Subtype) %>% 
  ggplot(aes(Subtype,value,))+
  stat_boxplot(geom="errorbar",position = position_dodge(0.2),width=0.2)+
  geom_boxplot(position = position_dodge(0.2),width=0.4)+
  geom_jitter(shape=16,size=2,position=position_jitter(0.2),aes(fill=Subtype,alpha=value,color=Subtype))+
  scale_size_continuous(range=c(1,3))+
  geom_signif(comparisons = list(c("NE","Non NE")),
              map_signif_level = T,textsize=4,vjust=0.5,hjust=0.5,color="black",test=wilcox.test)+
  facet_grid(.~name,scale="free",switch = "x")+
  scale_fill_aaas() +
  scale_color_aaas() +
  labs(y="Log2(TPM_Fpkm+1)")+
  theme_classic()+
  theme(axis.ticks.x=element_blank(),
        axis.text.x =element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_text(size=10),
        axis.line=element_line(color="black"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size=0.2,color="#e5e5e5"),
        axis.title.y = element_text(margin = margin(r = 12),color="black",size=10),
        plot.margin = unit(c(0.3,0,0.2,0.2),"cm"),
        legend.position = "non",
        strip.background = element_blank(),
        panel.spacing = unit(0,"lines"))

pal <- c("#E64B35FF","#4DBBD5FF","#F39B7FFF","#3C5488FF",
         "#8491B4FF","#91D1C2FF","#FF0000","#4DBBD5FF","#00A087FF")

c <- read_tsv("F3-c.xls") %>% left_join(.,read_tsv("F3-a.xls"),by="Sample_id") %>%
  left_join(.,read_tsv("F3-b.xls"),by="Sample_id") %>% select(Sample_id:Subtype.x,Subtype.y) %>% 
  pivot_longer(-c(Subtype.x,Subtype.y,Sample_id)) %>% 
  ggplot(aes(Sample_id,value,fill=name))+
  geom_col(position="stack")+
  facet_nested(.~Subtype.x+Subtype.y,drop=T,scale="free",space="free",switch="x",
               strip =strip_nested(background_x = elem_list_rect(fill =pal),by_layer_x = F))+
  scale_fill_jama()+
  labs(x=NULL, y="Relative percent")+
  scale_y_continuous(expand = c(0,0),labels=scales::percent)+
  theme(strip.background = element_rect(fill="white",color="black"),
        panel.spacing.x = unit(0.1,"lines"),
        strip.text.x = element_text(size=10,color="black"),
        axis.text.y=element_text(size=10,color="black"),
        axis.title.y = element_text(size=10,color="black"),
        axis.text.x = element_blank(),axis.ticks.x = element_blank(),
        legend.key=element_blank(), legend.text = element_text(color="black",size=10),
        legend.spacing.x=unit(0.1,'cm'),legend.key.width=unit(0.5,'cm'),
        legend.key.height=unit(0.5,'cm'), legend.background=element_blank(),
        legend.title = element_blank(),
        legend.position = "top",
        panel.grid.major=element_blank(),panel.grid.minor=element_blank())

(a|c)+plot_layout(ncol=2,widths = c(0.7,1))

  
  
