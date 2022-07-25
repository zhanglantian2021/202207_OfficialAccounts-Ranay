library(tidyverse)
library(ggforce)
library(glue)
library(ggtext)

df <- readr::read_csv("data.txt")

data <- df %>%
  filter(state %in% state.abb) %>%
  count(incident_month, incident_year, species_id, species) %>%
  arrange(species_id, species, incident_year, incident_month) %>%
  group_by(species, species_id, incident_year) %>%
  mutate(percent = n/sum(n)) %>%
  mutate(percent = ifelse(is.nan(percent), 0, percent)) %>%
  mutate(angle = 90 - (incident_month-1)*30,
         angle = ifelse(angle < 0, 360 + angle, angle),
         radians = angle*pi/180,
         x0 = percent * cos(radians),
         y0 = percent * sin(radians))


axes_lines <- function(radius) {
  
  tibble(segment = 1:6,
         x = c(0, radius*cos(pi/3), radius*cos(pi/6), radius,radius*cos(pi/6), radius*cos(pi/3)),
         xend = c(0, -radius*cos(pi/3), -radius*cos(pi/6), -radius, -radius*cos(pi/6), -radius*cos(pi/3)),
         y = c(radius, radius*sin(pi/3), radius*sin(pi/6), 0, -radius*sin(pi/6), -radius*sin(pi/3)),
         yend = c(-radius, -radius*sin(pi/3), -radius*sin(pi/6), 0, radius*sin(pi/6), radius*sin(pi/3)))
}

axes_labels <- function(radius) {
  tibble(month = 1:12,
         label = month.abb[month],
         x = c(axes_lines(radius)$x, axes_lines(radius)$xend),
         y = c(axes_lines(radius)$y, axes_lines(radius)$yend))  }


data %>%
  filter(species == "Crows") %>%
  ggplot(aes(x = incident_month, y = percent, group = incident_year, 
             color = incident_year, fill = incident_year)) +
  geom_segment(data = axes_lines(2), aes(x = x, xend = xend, y = y , yend = yend), 
               size = 0.1, color = "#cccccc", inherit.aes = FALSE) +
  geom_circle(aes(x0 = 0, y0 = 0, r = 2), inherit.aes = FALSE, size = 0.1, color = "#cccccc") +
  geom_circle(aes(x0 = 0, y0 = 0, r = 1), inherit.aes = FALSE, size = 0.1, color = "#cccccc") +
  geom_ellipse(aes(x0 = x0, y0 = y0, a = percent, b = percent/2, angle = radians, 
                   fill = incident_year), alpha = 0.5, size = 0.1)+
  geom_text(data = filter(axes_labels(2.15), !label %in% c("Mar", "Sep")), 
            aes(x = x, y = y, label = label), inherit.aes = FALSE) +
  scale_color_viridis_c() +
  scale_fill_viridis_c() +
  labs(x = NULL, y = NULL) +
  coord_fixed(clip = "off") +
  theme_void()+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none")

