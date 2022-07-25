library(tidyverse)
# devtools::install_github("johannesbjork/LaCroixColoR")
library(LaCroixColoR)

imdb <- readr::read_csv('imdb.txt')

imdb %>%
  ggplot(aes(y=fct_rev(factor(season)), x=factor(ep_num))) +
  geom_point(aes(color=rating, size=rating_n)) +
  scale_color_gradientn(colors=LaCroixColoR::lacroix_palette("PeachPear", type="continuous"),
                        breaks=c(3.9,6,8,9.8))+ 
  scale_size_area(breaks=c(2808, 10000, 19688),label=scales::comma) +
  coord_fixed() +
  theme_minimal() +
  theme(plot.margin=margin(0.1,0.1,0.3,0.3,unit="cm"),
        panel.grid=element_line(linetype = "dotted", color="grey60",size=.2),
        legend.text=element_text(size=7.5,color="black"),
        legend.direction = "vertical",
        legend.box = "horizontal",
        axis.text=element_text(color="black",size=8)) +
  guides(color=guide_colorbar(barheight = unit(14, "lines"),
                              barwidth = unit(.8, "lines"),order=1),
         size=guide_legend(override.aes = list(shape=21))) +
  labs(x=NULL,y=NULL,color=NULL,size=NULL)
