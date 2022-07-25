library(tidyverse)
library(ggridges)
library(extrafont)
library(geomtextpath)
library(gghalves)
library(magrittr)


chocolate <- readr::read_csv('chocolate.txt')


char <- chocolate %>% select(most_memorable_characteristics, rating)

char_single <- char %>% 
  mutate(most_memorable_characteristics = strsplit(as.character(most_memorable_characteristics), ", ")) %>% 
  unnest(most_memorable_characteristics)


char_single %<>% 
  group_by(most_memorable_characteristics) %>% 
  mutate(n = n(), 
         avg = mean(rating), 
         rating_diff = rating - mean(char_single $rating)) %>% 
  arrange(avg) %>% 
  filter(n > 60)


uniq <- unique(char_single$most_memorable_characteristics)

# convert to factor
char_single$most_memorable_characteristics <- as.factor(char_single$most_memorable_characteristics)

# create palette
myPalette <- colorRampPalette(c("#E41A1C", "#1E90FF", "#FF8C00"))

# ridge plot
char_single%>% 
  arrange(-avg) %>%
  mutate(most = fct_relevel(most_memorable_characteristics, uniq)) %>%
  ggplot(aes(x = rating, y = most, fill = avg)) +
  geom_density_ridges(alpha = 0.6, color = "#362a21") +
  scale_fill_gradientn(colours = myPalette(100)) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.ticks = element_blank(),
        axis.text.y = element_text(hjust = -0, color = "black", vjust = -0.45,
                                   margin = margin(l = 10, r = -50, unit = "pt")),
        axis.title = element_blank(),
        axis.text = element_text(size = 10)
  )

# polar half-boxplot, half-violin plot
char_single %>% 
  arrange(-avg) %>%
  mutate(most = fct_relevel(most_memorable_characteristics, uniq)) %>%
  ggplot(aes(x = most, y = rating, fill = avg)) +
  geom_half_boxplot(alpha = 0.4, color = "#544336", nudge = 0.06, outlier.shape = NA,width = 0.7)+
  geom_half_violin(alpha = 0.9, color = "#544336", side = "r", nudge = 0.02, width = 0.7) +
  scale_fill_gradientn(colours = myPalette(100)) +
  ylim(1, 4)+
  coord_curvedpolar() +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white"),
        axis.ticks = element_blank(),
        axis.text.x = element_text(size = 12, hjust = 0.5, color = "black", 
                                   vjust = 0, margin = margin(l = 10, r = -50, unit = "pt")),
        axis.text.y = element_blank(),
        axis.title = element_blank()
  )
