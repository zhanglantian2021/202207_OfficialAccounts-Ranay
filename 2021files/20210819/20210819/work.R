package.list=c("tidyverse","gganimate","extrafont","ggtext","ggsci")

for (package in package.list) {
  if (!require(package,character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

brazil_loss <- read_csv('brazil_loss.csv')

brazil <- brazil_loss %>% 
  select(-c(entity, code)) %>%
  pivot_longer(cols = -year, names_to = "reason",
               values_to = "amount") %>% 
  mutate(year = as.integer(year),
         reason = fct_reorder(reason, amount))

brazil_animated <- brazil %>% 
  ggplot() +
  aes(x = reason, y = amount, fill = reason) +
  geom_bar(stat = "identity") +
  coord_cartesian(clip = "off") +
  coord_flip() +
  scale_fill_nejm()+
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(colour = "black"),
        axis.text.x = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        text = element_text(size =15,color = "black"),
        plot.title = element_markdown(hjust = 0.5, size =20),
        legend.position = "none",
        plot.margin = margin(t = 1, r = 5, b = 1, l = 1, unit = "cm")) +
  labs(title = 'Sources of **deforestation in Brazil** in {frame_time}',
       x = NULL, y = NULL) +
  transition_time(year) +
  ease_aes('linear')

animate(brazil_animated, height = 600, width = 800)

anim_save("22_deforestation.gif")
