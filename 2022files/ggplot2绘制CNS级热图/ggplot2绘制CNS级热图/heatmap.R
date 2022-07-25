library(tidyverse)
library(ggtext)
library(ggforce)

read_tsv("data.xls") %>% 
  mutate(
    CL_evolution_sign = case_when(
      CL_evolution > 0 ~ "↑",
      CL_evolution < 0 ~ "↓",
      TRUE ~ "="
    ),
    PR_evolution_sign = case_when(
      PR_evolution > 0 ~ "↑",
      PR_evolution < 0 ~ "↓",
      TRUE ~ "="
    )
  ) %>% 
  ggplot() + 
  geom_circle(aes( x0= 1, y0 = 1, r = .5, fill = 8-CL), size = .125) +
  geom_circle(aes( x0= 2, y0 = 1, r = .5, fill = 8-PR), size = .125) +
  geom_text(aes(x = 1, y = 1, label = CL_evolution_sign), color = "#111111", size = 3.5) + 
  geom_text(aes(x = 2, y = 1, label = PR_evolution_sign), color = "#111111", size = 3.5)+
  facet_grid(rows = vars(country), cols =vars(year), switch = "y") +
  coord_equal() + 
  colorspace::scale_fill_continuous_diverging(name=NULL,palette = "Red-Green", mid = 4,
                                              guide = guide_colourbar(
                                                barwidth =unit(6, "cm"),
                                                barheight = unit(.75, "cm"),
                                                label.theme = element_text(size = 15)),
                                              breaks = c(2,6),
                                              labels = c("← Bad","Good →")
  ) +
  theme_minimal() +
  theme(
    text = element_text(),
    strip.placement = "outside", 
    strip.text.y.left = element_text(face = "bold", color = "#111111",size = 12,angle = 0, hjust = 1),
    strip.text.x =  element_text(face = "bold", color = "#111111", size = 12),
    panel.spacing.x = unit(.25, "cm"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.direction = "horizontal", 
    legend.position = "top"
  )

