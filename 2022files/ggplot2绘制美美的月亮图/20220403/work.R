library(tidyverse)
library(gggibbous)
library(ggtext)
library(ggfx)
library(showtext)
library(magick)

showtext_auto()
font_add_google("Karla", "Karla")
font_add_google("Oswald", "Oswald")

months_order <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
selected_cities <- c('Oslo',"Vienna","Brussels","Zagreb","Prague", "Copenhagen","Helsinki", "Paris","Dublin", "Athens","Budapest","Berlin","Rome","Milan","Amsterdam",
"Nicosia","Madrid","Lisbon","Istanbul","Moscow","Zurich","London","Reykjavik","Edinburgh","Tromsø","Tórshavn")

average_sunshine <- read_csv('average_sunshine_europe.csv')

average_monthly_sunshine <- average_sunshine %>%
  filter(City %in% selected_cities)   %>%
  pivot_longer(cols = Jan:Dec,
               values_to = 'sunshine_hours',
               names_to = 'month') %>%
  mutate(total_hours = case_when(
    month == 'Feb' ~ 28 * 24,
    month %in% c('Jan', 'Mar', 'May', 'Jun', 'Jul', 'Aug', 'Oct', 'Dec') ~ 31 *24,
    TRUE ~ 30 * 24)) %>%
  mutate(dark_hours = total_hours - sunshine_hours,
         ratio = sunshine_hours / total_hours) %>%
  mutate(month = factor(month, levels = months_order)) %>%
  mutate(right = TRUE)

g <- average_monthly_sunshine %>% ggplot() +
  geom_moon(aes(1,1,ratio = 1),fill = '#023047',color = '#023047',size = 15,alpha = 0.9) +
  geom_moon(aes(1, 1,ratio = 1),fill = '#e5e5e5',color = '#e5e5e5',size = 15,alpha = 0.3)+
  with_outer_glow(geom_moon(aes(1, 1,ratio = ratio, right = right),fill = '#fca311',color = '#fca311',
    size = 15),color = '#fca311')+
  geom_text(aes(1,1,label = round(sunshine_hours)),color = 'grey80',size = 3) +
  coord_fixed()  +
  theme_void(base_family = 'Oswald') +
  facet_grid(fct_reorder(City, Year) ~ month, switch = "y") +
  theme(legend.position = "none")

ggsave("01_part_to_whole.pdf", width = 10, height = 20, device = cairo_pdf)

g





















