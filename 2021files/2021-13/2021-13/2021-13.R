library(tidyverse)
library(widyr)
library(tidygraph)
library(ggraph)
library(rnaturalearth)
library(cowplot)
library(extrafont)



data <- read.csv("unvotes.csv")

unvotes <- data %>%
  left_join(countrycode::codelist %>% select(country_code = iso2c,continent))

euro = unvotes %>% filter(continent == "Europe")

europe = rnaturalearth::ne_countries(continent = "Europe",
                                     scale = "medium") %>% sf::st_as_sf()



euro_corrs = euro %>%
  mutate(vote = if_else(vote == "yes", TRUE, FALSE)) %>%
  pairwise_cor(tbl = ., item = country_code,
               feature = rcid, value = vote)
View(tidytab)
tidytab = euro_corrs %>%
  filter(correlation > .5) %>%
  as_tbl_graph(directed = F) %>%
  mutate(block = as.factor(group_edge_betweenness(weights = correlation)))


graph = ggraph(tidytab, layout = "kk") +
  geom_edge_link(aes(alpha = correlation)) +
  geom_node_point(shape = 21, aes(fill = block), color = "white", size = 12, stroke = 1.5) +
  geom_node_text(aes(label = name), color = "black") +
  theme_graph(base_family = "sans") +
  theme(legend.position = "none")



blocks = tidytab %>% 
  as.data.frame() %>% 
  tibble() %>%
  rename(iso_a2 = name) %>%
  left_join(select(unvotes, iso_a2 = country_code)) %>%
  distinct()

euro_blocks = europe %>%
  left_join(blocks)

map = ggplot(euro_blocks) +
  geom_sf(aes(fill = block)) +
  scale_x_continuous(limits = c(-40, 85)) +
  scale_y_continuous(limits = c(35, 80)) +
  theme_void() +
  theme(legend.position = "none", panel.border = element_rect(fill = NA))

map

ggdraw(graph) +
  draw_plot(map, x = .75, y = .75, scale = .4, hjust = .5, vjust = .5)

