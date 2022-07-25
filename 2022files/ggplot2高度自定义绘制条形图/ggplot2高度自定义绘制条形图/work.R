library(tidyverse)
library(ggtext)

red <- "#EE3224"
blue <- "#007DB2"

df <- readr::read_csv('transit_cost.txt') %>% 
  filter(country %in% c("US", "CA")) %>%
  mutate(country = case_when(country=="US" ~ "U.S.", 
                             country=="CA" ~ "Canada"),
         cost_km_millions = as.numeric(cost_km_millions)) %>%
  filter(!is.na(cost_km_millions), cost_km_millions > 0,
         !is.na(stations), stations > 0,end_year <= 2021) %>%
  select("country", "city", "line", "length", "stations",
         "cost_km_millions", "end_year") %>%
  arrange(desc(cost_km_millions)) %>%
  mutate(name = paste0("**", line,"**   |   ", city),
         name = fct_reorder(name, cost_km_millions))

df_stations <- df %>% 
  mutate(n_stations = stations)  %>%
  uncount(stations) %>%
  group_by(name) %>%
  mutate(row_num = row_number()) %>%
  ungroup() %>%
  mutate(
    length_frac = length/(2*(n_stations + 1)), 
    station_pos = length_frac*2*row_num) 

df_lines <- df %>% 
  mutate(zero = 0, start=0.5, end=length) %>% 
  select(name, country, start, end,zero,  cost_km_millions) %>%
  pivot_longer(cols=c(start, end, zero), names_to="v") 

ggplot() + geom_line(
    data = df_lines %>% filter(v %in% c("zero", "start")),
    aes(x=value, y=name, group=name, 
      size=cost_km_millions, color=country)) + 
  geom_line(data = df_lines%>% filter(v %in% c("start", "end")),
    aes(x=value, y=name, group=name, size=cost_km_millions, color=country),  
    lineend="round") + 
  geom_point(data = df_stations,aes(x=station_pos,y=name, 
      size=cost_km_millions, color=country),
    fill="white",shape=22) + 
  scale_color_manual(values=c("U.S." = blue, "Canada" = red)) + 
  theme_classic() + 
  labs(X=NULL,y=NULL)+
  theme(
    legend.position = "none", 
    axis.text.y = ggtext::element_markdown(family=font,size=10, color="grey30"), 
    axis.text.x = element_text(family=font,size=10, color="grey30"), 
    axis.title = ggtext::element_markdown(family=font, color="grey30"),
    panel.grid.major.x =element_blank(),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(0,20,0,0))+
  scale_x_continuous(expand=expansion(0))


