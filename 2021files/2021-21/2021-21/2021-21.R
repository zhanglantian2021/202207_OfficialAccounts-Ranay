library(tidyverse)
library(ggtext)
library(datasets)

us_states <- map_data("state")
degree_earning_map <- read.delim("degree_earning_map.xls",header = T,sep="\t",check.names = F)

ggplot() +
  geom_polygon(
    data = us_states,
    aes(
      x     = long, 
      y     = lat, 
      group = group
    ),
    fill  = "grey90",
    size = .75,
    color = rgb(151,151,151,50,maxColorValue=255)
  )  + 
  geom_text(
    data = degree_earning_map, 
    aes(
      x = longitude,
      y = latitude,
      label = state
    ),
    vjust = 1.2,
    hjust = 0.2,
    fontface = "bold",
    size = 3.5
  )+
  geom_segment(
    data = filter(degree_earning_map, education_level == "normal_sup_education"),
    aes(
      x = longitude, 
      xend = longitude,
      y = latitude, 
      yend = latitude + round(avg_annual_salary) / 70000 
    ),
    size = 4,
    color = "#00496f"
  )+
  geom_segment(
    data = filter(degree_earning_map, education_level == "long_sup_education"),
    aes(
      x = longitude+0.5, 
      xend = longitude+0.5,
      y = latitude, 
      yend = latitude + round(avg_annual_salary) / 70000 
    ),
    size = 4,
    color = "#dd4124"
  )+
  geom_text(
    data = filter(degree_earning_map, education_level == "normal_sup_education"),
    aes(
      x = longitude, 
      y = latitude + round(avg_annual_salary) / 70000,
      label=   paste0(round(avg_annual_salary/1000),"K")
    ),
    vjust = -.3,
    size = 2.8,
    color = "black",
    fontface = "bold"
  ) + 
  geom_text(
    data = filter(degree_earning_map, education_level == "long_sup_education"),
    aes(
      x = longitude+0.8, 
      y = latitude + round(avg_annual_salary) / 70000,
      label=   paste0(round(avg_annual_salary/1000),"K")
    ),
    hjust = .6,
    vjust = -.3,
    size = 2.8,
    color = "black",
    fontface = "bold"
  )+
  annotate(
    geom = "richtext",
    x = -95, y = 51,
    label = '<span style= "font-size:30px;"> COMPUTING OR TECH INDUSTRY</span><br><br>',
    fill = NA, label.color = NA,
    size = 5,
    fontface = "bold"
  )  +
  coord_cartesian(clip = "off") +
  theme_void() + 
  theme(
    plot.background = element_rect(fill = "white"),
    plot.margin = margin(t = 30)
  )
