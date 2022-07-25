package.list=c("tidyverse","ggvegan","ggsci","ggtext")

for (package in package.list) {
  if (!require(package,character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}


otu <- read.delim("otu.xls",check.names = F,row.names = 1) %>% 
  select(-taxonomy) %>% 
  t() %>% as.data.frame()

group <- read.delim("group.xls",check.names = F,row.names = 1)

set.seed(123)

anosim(otu,group$Group,distance = "bray",
       permutations = 999) %>% summary()

df <- anosim(otu,group$Group,distance = "bray",
            permutations = 999) %>% fortify()

ggplot(df,aes(x = Class,y = Rank,fill=Class)) +
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1,fill="white")+
  theme_bw()+
  scale_fill_nejm()+
  labs(title = "R<sup></sup>=0.1664 p_value=0.0039")+
  theme(legend.position = "non",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x= element_text(size=10,color = "black"),
        axis.text.y= element_text(size=10,color = "black"),
        plot.margin=unit(c(0.5,0.5,0.5,0.5),units=,"cm"),
        panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank(),
        panel.background=element_rect(colour="black",
                                      size=1,fill="white"),
        plot.title = element_markdown(vjust = 0.5,hjust = 0.5))
