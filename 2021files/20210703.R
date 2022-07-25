library(tidyverse)
library(ggtext)
library(glue)
library(ggsci)

data <- tibble(term = c("A","B","C","D"),
                      interest = c(5, 12, 2, 18),
                      x = rep(0, 4),
                      y = 0:3*-0.3) %>%
  rowwise() %>%
  mutate(color = if_else(term == "A", "firebrick",
                         sample(grey.colors(1000),1)))

ticks <- tibble(x = seq(0, 20, 5),xend = x,
                y = -1.1,yend = -1.12)

ticks

ggplot(data,aes(x=0-0.3,y=y)) +
  geom_text(label = c("2021-7","2021-6","2021-5","2021-4"), 
            size=4, hjust = 1.1, vjust = 0.5)+
  geom_segment(aes(x = x,xend=interest,y=y,
                   yend = y,color=color),
               size =20,alpha=0.5,show.legend = FALSE)+
  geom_text(aes(label=term,x=interest,y=y,color=color),
                hjust = 0,size=4,vjust = 0.5,show.legend = F)+
  geom_segment(x = 0, xend = 0, y = -1.1, yend =0.5,size=0.5)+
  geom_segment(data = ticks,aes(x = x, xend = xend,
                                y = y, yend = yend))+
  geom_segment(y = -1.1, yend=-1.1,x = 0,xend = 20,size = 1)+
  geom_text(data =ticks, aes(x = x, y = yend, label = seq(0,20,5)),
            vjust = 1.1)+
  scale_color_lancet()+
  labs(x = NULL,y = NULL)+
  expand_limits(x = c(-4,20), y = c(-1.2, 0.5))+
  theme_minimal()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())
  
