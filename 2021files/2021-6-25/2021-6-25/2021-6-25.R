library(tidyverse)
library(lubridate)
library(ggthemes)
library(scales)

stocked <- readr::read_csv('stocked.csv')
options(scipen = 999)

stocked %>%
  filter(SPECIES %in% c("BKT", "BNT", "LAT", "RBT"), 
         LAKE %in% c("HU", "ER", "MI", "ON", "SU")) %>%
  group_by(SPECIES, YEAR, LAKE) %>%
  summarize(year_total = sum(NO_STOCKED, na.rm = TRUE)) %>%
  mutate(year_total = round(year_total/1000000, 1), 
         LAKE = recode(LAKE, "ER" = "Erie", "HU" = "Huron", "MI" = "Michigan",
                       "ON" = "Ontario", "SU" = "Superior"), 
         SPECIES = recode(SPECIES, "BKT" = "Brook Trout", "BNT" = "Brown Trout",
                          "LAT" = "Lake Trout", "RBT" = "Rainbow Trout")) %>%
  rename(`Species` = `SPECIES`) %>%
  ggplot(aes(x=YEAR, y=year_total, color = Species)) + 
  geom_line() + 
  labs(x =NULL, y =NULL) +
  scale_y_continuous(labels = comma_format(big.mark = ",",
                                           decimal.mark = ".")) + 
  facet_wrap(~LAKE, ncol =3, scale = "free_y")+
  theme_minimal() + 
  scale_color_colorblind()+ 
  theme(legend.position = c(1, 0),
        legend.justification = c(1, 0), 
        plot.title = element_text(size = 15))