package.list=c("tidyverse","ggsci","ggh4x","aplot","RColorBrewer","ggpubr","cowplot","grid","ggplotify")

for (package in package.list) {
  if (!require(package,character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

color <- c("white","#709AE1FF","#8A9197FF","#D2AF81FF","#FD7446FF",
           "#D5E4A2FF","#197EC0FF","#F05C3BFF","#46732EFF",
           "#71D0F5FF","#370335FF","#075149FF","#C80813FF","#91331FFF",
           "#1A9993FF","#FD8CC1FF")

data <- read_tsv('data.txt',col_names=F) %>% 
  select(1,2,6) %>% 
  group_by(X1,X6) %>% 
  count(X2) %>% ungroup() %>% select(-2) %>% 
  pivot_wider(., names_from =X2,values_from = n) %>% 
  mutate_all(~replace(.,is.na(.), 0)) %>% 
  pivot_longer(-X1) %>% 
  mutate(value=as.character(value),signif=value,
         signif=case_when(value=="0" ~ " ",TRUE ~ as.character(value)),
         name=case_when(name=="Box II -like sequence" ~ 
                          "Box II -like sequence  ",
                        TRUE ~ as.character(name)),
         n=str_remove(X1,"gene") %>% as.numeric()) %>%
  arrange(n)
data

df <- data %>% select(name) %>% distinct() %>% 
  mutate(group= rep(LETTERS[1:3],times=c(8,6,9)
                  )) %>% left_join(.,data,by="name")

df$X1 <- factor(df$X1,levels=df$X1 %>% rev() %>% as.data.frame() %>% 
                  distinct() %>% pull())

p1 <- df %>% ggplot(.,aes(interaction(name,group),X1,color=value,fill=value))+
  geom_tile(color="grey80",fill="white",size=0.5)+
  geom_point(pch=22,size=5)+
  geom_text(aes(label=signif),size=3,color="black")+
  guides(x="axis_nested")+
  labs(x = NULL,y = NULL,color=NULL)+
  scale_color_manual(values=color)+
  scale_fill_manual(values=color)+
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0),position="left")+
  theme(axis.text.x=element_text(color="black",angle=90,size=8,hjust=0),
        axis.text.y=element_text(color="black",size=8),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.border=element_rect(fill=NA,color="grey80",size=1,linetype="solid"),
        ggh4x.axis.nestline.x = element_line(size = 1),
        ggh4x.axis.nesttext.x = element_text(colour = "blue",angle =0),
        legend.position = "non",
        plot.margin=unit(c(0.2,0.2,0.2,0.2),units=,"cm"))+
  scale_x_discrete(expand = c(0,0),position = 'top')

mycolors <- colorRampPalette(brewer.pal(8, "Set1"))(23)

p2 <- df %>% select(name,X1,value) %>% mutate(value=as.numeric(value)) %>% 
  ggplot(aes(value,X1,fill=name))+
  geom_col(position="stack",width=0.5)+
  labs(x=NULL,y=NULL)+
  scale_fill_manual(values=mycolors)+
  scale_x_continuous(expand = expansion(0),position = 'top')+
  theme_test()+
  theme(axis.text.x=element_text(color="black",size=8,hjust=0),
        axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.border=element_rect(fill=NA,color="grey70",size=1,linetype="solid"),
        ggh4x.axis.nestline.x = element_line(size = 1),
        legend.position = "non",
        legend.key=element_blank(),   
        legend.title = element_blank(),
        legend.text = element_text(color="black",size=5), 
        legend.spacing.x=unit(0.1,'cm'),
        legend.spacing.y=unit(0.1,'cm'), 
        legend.key.width=unit(0.3,'cm'), 
        legend.key.height=unit(0.3,'cm'), 
        legend.background=element_blank(),
        plot.margin=unit(c(0.2,0.2,0.2,0.2),units=,"cm"))+
  guides(fill = guide_legend(direction = "horizontal"))+
  guides(fill=guide_legend(nrow=6, byrow=TRUE)) 

p3 <- p2 + theme(legend.position = "top")

p1 %>% insert_right(p2,width=.6) %>% as.grob() %>% ggdraw()+
  draw_plot(ggpubr::get_legend(p3) %>% as_ggplot(),scale=0.05,x=0.32,y=0.38)

ggsave("gene.pdf",width=8.39,height=5.95,units="in",dpi=300)




