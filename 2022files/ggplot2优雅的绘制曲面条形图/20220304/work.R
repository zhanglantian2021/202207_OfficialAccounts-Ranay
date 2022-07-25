library(tidyverse)

food_consumption <- readr::read_csv('food_consumption.csv')

food_co2 <- food_consumption %>% 
  filter(country !="Hong Kong SAR. China") %>% 
  group_by(country) %>% 
  summarise(co2 = sum(co2_emmission)) %>% 
  top_n(., 15, co2) %>% 
  arrange(co2) %>% 
  mutate(n = -5:9) %>% 
  rowwise() %>%
  mutate(x = list(c(-20, 0, 0, -20)),
         y = list(c(n*4 - 1.4, n*2 - 0.7, n*2 + 0.7, n*4 + 1.4))) %>% 
  unnest(cols = c(x, y)) %>% as.data.frame()

ggplot(food_co2) +
  geom_rect(aes(xmin = -42, ymin = n*4 - 1.4,
                xmax = -20, ymax = n*4 + 1.4), fill = "grey80", color = NA) +
  geom_polygon(aes(x, y, group = n), fill = "grey70", color = NA) +
  geom_rect(aes(xmin = 0, ymin = n*2 - 0.7,
                xmax = co2/30, ymax = n*2 + 0.7), fill = "grey", color = NA) +
  geom_text(aes(-40.5, n*4, label = country),color = "white", hjust = 0, size =6,
            check_overlap = TRUE) +
  geom_text(aes(co2/30-1, n*2, label = co2),color = "black",hjust = 1,
            size = 4, check_overlap = TRUE) +
  scale_x_continuous(breaks = seq(0,80,20),labels = seq(0,2000,500)) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 12),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_line(color = "grey80", size = 0.2),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.margin = margin(10,10,10,10)) 
