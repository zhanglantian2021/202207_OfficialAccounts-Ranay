library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(datawizard)
library(MetBrewer)
library(ggbump)

theme_niwot <-  function() {
  theme_void() +
    theme(
      plot.background = element_rect(fill="lightblue1"),
      plot.title = element_text(size = 18, hjust=0.5),
      axis.text.y.left = element_blank(),
      axis.text.x.bottom = element_blank(),
      legend.position = "none"
    )
}

world <- ne_countries(scale = "medium", returnclass = "sf")

map <- world %>% 
  filter(continent == "Europe", iso_a2 != "RU") %>% 
  st_crop(xmin = -24, xmax = 40, ymin = 33, ymax = 74)

ggplot() +
  geom_sf(data=map, color="lightblue1", fill="forestgreen") +
  theme_niwot()

points <- map %>% st_point_on_surface() %>% st_coordinates() %>% as_tibble() %>% 
  mutate(country=map$name_long)

ggplot(points) +
  geom_sf(data=map, color="lightblue1", fill="forestgreen") +
  geom_point(aes(x=X, y=Y)) +
  geom_text(aes(x=X, y=Y,label=country),hjust=0, nudge_x=1, check_overlap=TRUE) +
  theme_niwot()

forest_area <- read_csv("data.txt") %>% filter(year == 2020) %>% 
  mutate(forest_ha = 4200000000 * (forest_area/100) / 1000000) %>% 
  select(entity, forest_ha)
  
line_data <-
  points %>% left_join(forest_area, by=c("country" = "entity")) %>% 
  slice_max(forest_ha, n=15) %>% 
  mutate(col_y = rank(forest_ha) %>% data_rescale(to=c(40, 70)),sigmoid_end = 52,
         col_x_start = 55,col_x_end = forest_ha %>% data_rescale(to=c(55, 130),range = c(0, max(forest_ha))),
         area_label = paste0(round(forest_ha, 1), " million ha"))
  
line_data %>% ggplot() +
  geom_sf(data=map, color="lightblue1", fill="forestgreen") +
  geom_point(aes(x=X, y=Y, color=forest_ha)) +
  geom_sigmoid(aes(x=X, y=Y, xend=sigmoid_end, yend=col_y, group=country, color=forest_ha)) +
  geom_text(aes(label=country, x=sigmoid_end, y=col_y),hjust=1, size=2, vjust=0, nudge_y = 0.5, alpha=0.8) +
  geom_segment(aes(x=col_x_start, xend=col_x_end, y=col_y, yend=col_y, color=forest_ha),size=3) +
  geom_text(aes(label=area_label, x=col_x_end, y=col_y), hjust=0, size=2.2, nudge_x = .3) +
  coord_sf(clip = "off") +
  theme_niwot()
  