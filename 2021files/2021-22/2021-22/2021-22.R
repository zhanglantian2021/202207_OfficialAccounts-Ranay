#devtools::install_github("rensa/ggflags")
library(tidyverse)
library(ggflags)
library(showtext)
font_add_google("Rubik")
showtext_auto()
library(ggforce)

records <- read_csv("records.txt")
drivers <- read_csv("drivers.txt") %>% 
  mutate(country = case_when(
    nation == "USA" ~ "us",
    nation == "Australia" ~ "au",
    nation == "Canada" ~ "ca",
    nation == "Netherlands" ~ "nl",
    nation == "UK" ~ "gb",
    nation == "Brazil" ~ "br",
    nation == "Germany" ~ "de",
    nation == "Austria" ~ "at",
    nation == "Croatia" ~ "hr",
    nation == "France" ~ "fr",
    nation == "Ireland" ~ "ie",
    nation == "Norway" ~ "no",
    nation == "Slovenia" ~ "si"
  )) %>% 
  select(player, nation, country) %>% 
  distinct()

records_n <- left_join(records, drivers, by = "player")


records_n %>%
  filter(track == "Wario Stadium",
         type == "Single Lap",
         !is.na(nation)) %>% 
  group_by(track) %>% 
  mutate(time_rel = time-max(time),
         time_rel_prop = (max(time)-time)/max(time)) %>% 
  ggplot(aes(date, time)) + 
  geom_line(alpha = 0.5) + 
  geom_flag(aes(country = country)) + 
  scale_country() + 
  scale_y_continuous(breaks = pretty) + 
  facet_zoom(ylim = c(85.75, 88)) + 
  theme_bw() + 
  labs(
    title = "Wario Stadium World Records, 1997-2021",
    y = "Track time (s)",
    x = NULL) + 
  theme(legend.position = "none",
        panel.grid.major.y = element_line(colour = "grey95"),
        panel.grid.minor.x = element_blank(),
        text = element_text(colour = "black", family = "Rubik", 
                            size = 10),
        axis.text = element_text(colour = "black", family = "Rubik"),
        axis.text.x = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        plot.title = element_text(size =20, face = "bold", hjust = 0.5),
        plot.margin = margin(5,5,5,5))
