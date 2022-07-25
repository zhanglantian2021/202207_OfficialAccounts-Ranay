library(tidyverse)
library(data.table)
library(gridExtra)
library(patchwork)

file_name <- "loop_data.tsv"
dat <- fread(file_name,sep="\t") %>% as.data.frame()
cities = unique(dat$city)
city_plots = list()
for(city_ in cities) {
  city_plots[[city_]] <-  ggplot(dat %>% filter(city == city_),aes(x=zone, y=`multistorey buildings`)) + 
    geom_bar(stat="identity",width=0.5) + 
    theme(axis.text.x = element_text(angle=0)) + 
    ggtitle(city_) + 
    labs(y=NULL,x=NULL)+
    theme(plot.title=element_text(size=15,face="bold",hjust=0.5),
          axis.title.x=element_text(size=10,face="bold"),
          axis.title.y=element_text(size=10,face="bold"))
  
  print(city_plots[[city_]])
  ggsave(city_plots[[city_]],file=paste0("plot_",city_,".pdf"), width =3.04, height =3.10, units = "in", dpi=300)
}

grid.arrange(grobs=city_plots,ncol=3)
patchwork::wrap_plots(city_plots,nrow=2)


