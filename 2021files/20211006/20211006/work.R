library(tidyverse)
library(ggtext)

load(file = "plot.RData")

ggplot(plot_data, aes(year,time, group = name))+
  geom_line(color = "grey80", size = 0.8)+
  geom_line(data = plot_data_main, size = 0.9,aes(color = name))+
  geom_point(data = plot_data_main, aes(fill = name), 
             size = 3, shape = 21, stroke = 1, color = "#F0F0F0")+
geom_text(data = filter(plot_data_main, year == 2020),
    aes(x = year + 0.4, label = str_replace(name,"Grand Prix", "GP"),color = name),
    size = 3.5, fontface = "plain",
    hjust = 0, nudge_y = 0.4)+
  scale_x_continuous(limits = c(1995,2021), breaks = seq(1995,2020,5))+
  scale_y_time(labels = function(x) strftime(x, "%M:%S"))+
  paletteer::scale_color_paletteer_d(palette = "colRoz::a_westwoodi") +
  paletteer::scale_fill_paletteer_d(palette = "colRoz::a_westwoodi")+
  coord_cartesian(clip = "off", expand = 0)+
  theme_minimal()+
  theme(legend.position = "none",plot.margin = margin(20,80, 10, 20),
        axis.text.x=element_text(color="black"),
        axis.text.y=element_text(color="black"),
        axis.title = element_blank())

