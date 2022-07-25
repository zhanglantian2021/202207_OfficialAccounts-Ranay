library(tidyverse)
library(magrittr)
library(hrbrthemes)
library(ggdark)
# BiocManager::install("wpp2015")
library(wpp2015)
library(ggridges)
library(patchwork)


data(UNlocations)

countries <- UNlocations %>% pull(name) %>% paste


data(e0M)

p1 <- e0M %>%
  filter(country %in% countries) %>%
  select(-last.observed) %>%
  gather(period, value, 3:15) %>%
  ggplot(aes(x = value, y = period %>% fct_rev()))+
  geom_density_ridges(aes(fill = period), color = "#eaeaea", size = .25)+
  geom_vline(xintercept = 0, color = "#eaeaea")+
  scale_fill_viridis_d(
    option = "mako", begin = .2
  )+
  scale_x_continuous(position = "top", expand = c(0,0))+
  labs(
    x = NULL, y = NULL,
    title = "Global convergence in mortality",
    subtitle = "Male life expectancy at birth"
  )+
  dark_theme_minimal(base_family =  font_rc, base_size = 14)+
  theme(
    legend.position = "none",
    plot.title = element_text(family = "Roboto Slab", size = 20, face = 2),
    axis.text = element_text(face = 2),
    plot.background = element_rect(fill = "#222222", color = NA),
    panel.grid.minor.x  = element_blank(),
    panel.grid = element_line(color = "#eaeaea77")
  )

p1
#------------------------------
data(e0F)

p2 <- e0F %>%
  filter(country %in% countries) %>%
  select(-last.observed) %>%
  gather(period, value, 3:15) %>%
  ggplot(aes(x = value, y = period %>% fct_rev()))+
  geom_density_ridges(aes(fill = period), color = "#eaeaea", size = .25)+
  geom_vline(xintercept = 0, color = "#eaeaea")+
  scale_fill_viridis_d(
    option = "rocket", begin = .2
  )+
  scale_x_continuous(position = "top", expand = c(0,0))+
  labs(
    x = NULL, y = NULL,
    subtitle = "Female life expectancy at birth",
    caption = "Data: UN WPP 2015"
  )+
  dark_theme_minimal(base_family =  font_rc, base_size = 14)+
  theme(
    legend.position = "none",
    plot.title = element_text(family = "Roboto Slab", size = 20, face = 2),
    axis.text = element_text(face = 2),
    plot.background = element_rect(fill = "#222222", color = NA),
    panel.grid.minor.x  = element_blank(),
    panel.grid = element_line(color = "#eaeaea77")
  )

p1
#------------
p1+p2


