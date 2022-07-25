library(tidyverse)
library(ggtext)
library(ggsci)

castaways <- readr::read_csv('castaways.txt')

df <- castaways %>%
  select(season, personality_type, order) %>%
  filter(!is.na(personality_type)) %>%
  separate(personality_type,
           into = c("mind","energy", "nature", "tactics"), 
           sep = 1:3) %>%
  group_by(season) %>%
  mutate(order_rev = pmin(18,max(order) - order + 1)) %>%
  ungroup()

df %>%
  count(order_rev, mind) %>%
  ggplot(aes(x = order_rev, y = n, fill = mind,
             col = mind,alpha = order_rev %in% c(1:3,15:18))) + 
  geom_col(position = "dodge")+
  scale_x_continuous(breaks = 1:18, labels = c(1:17, ">18"))+
  scale_fill_nejm()+
  scale_color_nejm()+
  scale_alpha_discrete() +
  theme_light() +
  labs(x=NULL)+
  scale_y_continuous(expand=c(0,0))+
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.subtitle = element_markdown(size = 16),
        plot.title = element_text(size = 20),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.position = "none")
  