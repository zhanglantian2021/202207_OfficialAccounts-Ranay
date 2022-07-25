library(scales)
library(ggtext)
library(forcats)
library(extrafont)
library(tidyverse)
library(tidytuesdayR)


tuesdata <- tidytuesdayR::tt_load(2021, week = 9)
employed <- tuesdata$employed

# plot ------------------------------------------
employed %>% mutate_if(sapply(employed, is.character), as.factor) %>% 
  filter(race_gender == "Men" | race_gender == "Women", 
         year == 2020,
         industry != "NA",
         industry != "Women",
         industry != "White") %>%
  ggplot(aes(x = industry_total, y = fct_rev(industry))) + 
  geom_line(color = "#d95f02", size = 1) + 
  geom_point(aes(color = race_gender), size = 5) +
  scale_color_manual(values = c("#1b9e77", "#7570b3")) +
  scale_x_continuous(labels = comma) +
  theme_minimal() +
  theme(legend.position = "none", 
        panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"), 
        plot.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"), 
        plot.title = element_markdown(size = 20), 
        plot.title.position = "plot",
        text = element_text(family = "Corbel")) +
  labs(y = "", 
       x = "Total employed", 
       title = "In most industries in 2020 in the USA,
       there were more <b style='color:#1b9e77'>men</b> employed than <b style='color:#7570b3'>women</b>.")
