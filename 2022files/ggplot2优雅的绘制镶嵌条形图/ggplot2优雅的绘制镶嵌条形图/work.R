library(tidyverse)
library(camcorder)
library(ggtext)

  
incl_gen_2019 <- read_tsv("incl_gen_2019.xls") %>% 
  mutate(OECD = rowMeans(select(., 3:last_col()))) %>% 
  rename(provisions = 1) %>% 
  add_row(provisions = "All provisions", !!! colMeans(.[-1], na.rm = TRUE)) %>% 
  pivot_longer(2:last_col(), names_to = "country") %>% 
  mutate(year = 2019, .before = 1)


incl_gen_1999 <- read_tsv("data1990.xls") %>%  
  mutate(OECD = rowMeans(select(., 3:last_col()), na.rm = TRUE)) %>% 
  janitor::remove_empty("cols") %>% 
  select(1, OECD, everything()) %>% 
  rename(provisions = 1) %>% 
  add_row(provisions="All provisions",!!!colMeans(.[-1],na.rm = TRUE)) %>% 
  pivot_longer(2:last_col(), names_to = "country") %>% 
  mutate(year = 1999, .before = 1)



all_2019 <- incl_gen_2019 %>% filter(startsWith(provisions, "All")) %>% 
  mutate(country = fct_reorder(country, value))

all_1999 <- incl_gen_1999 %>% filter(startsWith(provisions, "All")) 


ggplot() +
  geom_col(data = all_2019,
           aes(value, country,fill=if_else(country != "OECD", "purple4", "orange3")), width = 0.8) +
  geom_col(data = all_1999, aes(value, country,
                                fill = if_else(country != "OECD", "#BC83FD","#FCB651")), width = 0.3) +
  scale_x_continuous(sec.axis =sec_axis(~./2),breaks = seq(0,1,0.2),expand=c(0,0),
                     limits = c(0,1),labels = scales::label_percent(scale = 100)) +
  scale_fill_identity() +
  coord_flip()+
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.title = element_blank(),
    axis.title.x = element_blank(),
    axis.line = element_line(color = "#3D4852",size=1),
    axis.ticks = element_line(color = "#3D4852",size=1),
    panel.grid.major.y = element_line(color = "#DAE1E7"),
    panel.grid.major.x = element_blank(),
    plot.margin = unit(rep(0.2,4),"cm"),
    axis.title.y=element_blank(),
    axis.text.y = element_text(margin = margin(r=2),color="black"),
    axis.text.x = element_text(margin = margin(t=2,b=10),angle = 90,vjust=0.5,size=12,
                               hjust=1,color="black"),
    legend.position = "non")




