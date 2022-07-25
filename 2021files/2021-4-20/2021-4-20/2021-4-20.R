library(rnaturalearth)
library(tidyverse)
library(janitor)
library(scales)
library(magick)
library(rgeos)
library(sf)
library(cowplot)

co2_world_raw <- 
  read_csv("annual-co-emissions-by-region.txt") %>% 
  clean_names()

world_raw <- ne_countries(scale = 50, returnclass = "sf") %>% 
  clean_names()


world_sf <- world_raw %>% 
  select(iso_a3, continent) %>% 
  mutate(continent = case_when(
    continent == "Seven seas (open ocean)" ~ "Seven Seas",
    T ~ continent
  )) %>% 
  drop_na()

co2_world <- co2_world_raw %>% 
  inner_join(world_sf, by = c("code" = "iso_a3")) %>% 
  st_as_sf(crs = 4326)




top_15 <- co2_world %>% 
  filter(year == 2019) %>% 
  st_centroid() %>% 
  cbind(st_coordinates(.)) %>% 
  st_drop_geometry() %>% 
  mutate(entity = fct_lump(entity, 15, w = annual_co2_emissions)) %>% 
  filter(as.character(entity) != "Other")


area_df <- co2_world %>% 
  st_drop_geometry() %>% 
  bind_rows(
    co2_world_raw %>% 
      filter(entity == "International transport") %>% 
      mutate(code = "hi", continent = "Intl. Transport")
  ) %>% 
  mutate(
    continent = fct_reorder(continent, annual_co2_emissions, sum)
  ) %>% 
  arrange(continent, desc(annual_co2_emissions)) %>% 
  mutate(entity = fct_inorder(entity)) 



cols <- c(
  "Europe" = "#004559",
  "Asia" = "#017f98",
  "North America" = "#94bb1f",
  "Africa" = "#cfe006",
  "South America" = "#fce207",
  "Oceania" = "#f96e38",
  "Seven Seas" = "#f85700",
  "Intl. Transport" = "#f8a41a"
)


p1 <- co2_world %>% 
  filter(year %in% c(1850, 1950, 2019)) %>% 
  st_centroid() %>% 
  cbind(st_coordinates(.)) %>% 
  st_drop_geometry() %>% 
  ggplot(aes(
    X, Y, size = annual_co2_emissions, color = continent, label = entity
  )) +
  geom_point(alpha = .75) +
  geom_text(
    data = top_15, size = 3, check_overlap = TRUE, color = "#fffbe9") +
  scale_color_manual(values = cols) +
  scale_size_area(max_size = 35) +
  facet_wrap(~ year, ncol = 1) +
  coord_fixed() +
  theme_void(base_size = 20) +
  theme(
    legend.position = "none", 
    plot.margin = margin(t = 90, l = 180), 
    strip.text = element_text(size = 12, margin = margin(10,10,10,10)),
    panel.border = element_rect(fill = NA, color = "#fffbe9"),
    text = element_text( colour = "#fffbe9"),
  ) 

p2 <- area_df %>% 
  ggplot(aes(
    year, annual_co2_emissions, fill = continent, group = entity
  )) +
  geom_area(color = "#fffbe9", size = .1) +
  scale_fill_manual(values = cols) +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-9), position = "right") +
  guides(fill = guide_legend(nrow = 1, reverse = TRUE)) +
  coord_cartesian(xlim = c(1830, 2019)) +
  labs(
    x = NULL, y = NULL, 
    title = bquote(~CO[2]~"Emissions"),
    subtitle = "Billion Tonnes", 
    fill = NULL) +
  theme_minimal(base_size = 15) +
  theme(
    text = element_text(colour = "#fffbe9"),
    plot.title = element_text(size = 20,vjust = .5,hjust = .5),
    plot.title.position = "plot",
    plot.subtitle = element_text(hjust = 1),
    plot.background = element_rect("#1b1404", color = NA), 
    plot.margin = margin(20,20,20,20), plot.caption.position = "plot",
    legend.position = "bottom", 
    legend.text = element_text(size = 10), 
    panel.grid = element_blank()) 


ggdraw(p2)+
  draw_plot(p1, x = 0.05, y = .75, scale =.8, hjust = .5, vjust = .5)




