library(tidyverse)
  
  data <- read_tsv("data.xls")
  
  ggplot(data,aes(y = reorder(country, diff), x = diff, color=balance))+
    geom_segment(aes(yend = country), xend=0, size=5)+
    geom_point(size=4.5)+
    geom_vline(xintercept = 0, size=1, color='grey35')+
    expand_limits(x=c(-3,3.75))+
    scale_x_continuous(breaks = c(-2, -1, 1, 2))+
    geom_text(data=data %>% filter(diff <0),aes(label=country), x = -3.2, hjust=0, size=3.5, color="black")+
    geom_text(data=data %>% filter(diff >0),aes(label=country), x =2.2, hjust=0, size=3.5, color="black")+
    geom_segment(data=data %>% filter(diff <0),aes(yend = country), xend = -3.2, x=0, alpha = 0.2, size=5)+
    geom_segment(data=data %>% filter(diff >0),aes(yend = country), xend = 4.5, x=0,alpha = 0.2, size=5)+
    scale_color_manual(values=c("#BA7A70", "steelblue4"))+
    guides(color="none", y="none")+
    labs(x=NULL,y=NULL)+
    theme_minimal()+
    theme(plot.background = element_rect(fill = "Aliceblue", color="Aliceblue"),
          panel.grid = element_blank(),
          axis.line.x = element_line(color = "grey3", size = 0.5),
          panel.grid.major.x = element_line(linetype = "dashed", color = "grey3"),
          axis.text.x = element_text(size = 10, color = "black"))+
    geom_rect(xmax =3.8,xmin = 2, ymin = 0, ymax = 3, fill = "aliceblue", color ="steelblue4")+
    geom_segment(y=1,yend=1, x=2.6, xend=3.6, size=5, alpha=0.5, color='steelblue4')+
    geom_point(y=1, x = 2.6, size = 4.5, color = "steelblue4")+
    geom_text(label = "More", y = 1 ,x = 2.1, hjust = 0, color = "steelblue4", size = 3.5)+
    geom_segment(y=2.3, yend=2.3, x=2.6, xend=3.6, size=5, alpha=0.5, color='#BA7A70')+
    geom_point(y=2.3,x = 2.6, size = 4.5, color = "#BA7A70")+
    geom_text(label = "Less", y =2.3, x = 2.1, hjust = 0, color = "#BA7A70", size = 3.5)

    
  






