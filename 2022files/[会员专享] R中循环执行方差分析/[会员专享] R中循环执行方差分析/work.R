package.list=c("tidyverse","ggthemes","multcompView","magrittr","MetBrewer")

for (package in package.list) {
  if (!require(package,character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

df <- read_tsv("data.xls") %>% arrange(conc) %>% 
  unite(.,col="type",Treatment:Type,sep="_",
        remove = T,na.rm = F) %>% 
  mutate(Plant=str_remove(Plant,"[0-9]+"))
 
p <- split(df,list(df$conc))
aov_data <- data.frame()

for(i in 1:7) {
  anova <- aov(uptake ~ Type,data=p[i] %>% as.data.frame() %>% 
                 set_colnames(c("Plant","Type","conc","uptake")))
  
  Tukey <- TukeyHSD(anova)
  cld <- multcompLetters4(anova,Tukey)
  
  dt <- p[i] %>% as.data.frame() %>% 
    set_colnames(c("Plant","Type","conc","uptake")) %>% 
    group_by(Plant,Type,conc) %>%
    summarise(value_mean=mean(uptake),sd=sd(uptake)) %>%
    ungroup() %>% 
    arrange(desc(value_mean)) %>% 
    as.data.frame()
  
  cld <- as.data.frame.list(cld$`Type`)
  dt$Tukey <- cld$Letters
  
  aov_data  <- rbind(aov_data,dt)
}

aov_data %<>% arrange(Plant) %>% mutate(conc=as.character(conc)) 

aov_data$conc <- factor(aov_data$conc,levels=c("95","175","200","250","350","500","675","1000"))

aov_data %>% 
  ggplot(aes(conc,value_mean,fill=Type))+
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymax = value_mean + sd, ymin = value_mean - sd),
                position = position_dodge(0.9),width = 0.2,color = "Gray25")+
  scale_fill_manual(values=met.brewer("Monet",4))+
  scale_y_continuous(expand = expansion(0))+
  geom_text(aes(label=Tukey, y = value_mean + sd + 1.5), color = "white",
            show.legend = FALSE,position = position_dodge(0.9))+
  geom_text(aes(label=Tukey, y = value_mean + sd +0.8), size = 3, color = "black",
            show.legend = FALSE,position = position_dodge(0.9))+
  labs(x=NULL,y=NULL)+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(color="black",size=12,margin = margin(r=3)),
        axis.ticks.x=element_blank(),
        axis.text.y=element_text(color="black",size = 10,margin = margin(r =2)),
        axis.text.x=element_text(color="black",size = 10),
        panel.background = element_rect(fill = NA,color = NA),
        panel.grid.minor= element_line(size=0.2,color="#e5e5e5"),
        panel.grid.major = element_line(size=0.2,color="#e5e5e5"),
        panel.border = element_rect(fill=NA,color="black",size=0.3,linetype="solid"),
        legend.key=element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(color="black",size=8),
        legend.spacing.x=unit(0.1,'cm'),
        legend.key.width=unit(0.4,'cm'),
        legend.key.height=unit(0.4,'cm'),
        legend.position = c(0.18,1),legend.justification=c(1,1),
        legend.background=element_blank(),
        legend.box.margin = margin(0,0,0,0),
        strip.text = element_text(color="black",size=10),
        strip.background = element_blank(),
        panel.spacing.x=unit(0.3,"cm"))

