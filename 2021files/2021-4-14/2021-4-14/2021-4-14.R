library(tidyverse)
library(ggthemes)
library(cowplot)

accidents <- read.csv("accidents.txt")
accidents$Year <- as.numeric(accidents$Year)

ggplot(accidents, aes(x=Month, y=Year, fill=Deaths)) +
  geom_tile(colour="white") +
  scale_fill_gradient(low = "white", high = "#c85c32") + xlim(-11,13) + ylim(2000,2020) +
  coord_polar(theta = "x", start = pi/3)  + 
  theme(panel.background=element_rect(fill = "#f1eeea"),
        axis.title=element_blank(),
        panel.grid=element_blank(),
        axis.text.x = element_blank(),
        axis.ticks=element_blank(),
        axis.text.y=element_blank(),
        legend.position = c(0.48,0.52),
        legend.direction = "vertical",
        legend.background = element_blank(),
        legend.text = element_text(size = 8)) +
  geom_text(aes(x=12.5, y=Year, label=Year),hjust=-0.2,
            color="#b7b1ab", size=2.5, angle= -53,
            inherit.aes = FALSE, fontface = "plain") +
  geom_text(aes(x=1, y=2020, label="Jan"),
            hjust=0.5, vjust = 0, color="#fcfbfa", size=5,
            angle= 120, inherit.aes = FALSE, fontface = "plain") + 
  geom_text(aes(x=12, y=2020, label="Dec"),hjust=0.5, vjust = 0,
            color="#fcfbfa", size=5, angle= -44, inherit.aes = FALSE )+
  guides(fill = guide_colourbar(barheight = 2.5, barwidth = 1,
                                ticks= F,title.position = "left",
                                title.theme = element_text(size = 12, angle = 90),
                                label.theme = element_text(size = 7)))





