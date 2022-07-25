library(tidyverse)
library(extrafont)
library(patchwork)
library(ggExtra)

penguins <- palmerpenguins::penguins

labels <- tibble(species = c("Chinstrap", "Adelie", "Gentoo"),
                 species_x = c(190, 205, 228),
                 species_y = c(56, 38, 43))


penguin_hues <- c("#d2dbe4", "#8a5d24", "#646376","#192029", "#acb3bf", "#596e94")

theme_penguin_light <- function() {
  theme_minimal() %+replace%
    theme(text = element_text(colour = penguin_hues[4], family = "Arvo"), 
          axis.text = element_text(size = 8),
          axis.title = element_text(size = 10),
          axis.ticks = element_blank())
}


p <- ggplot(penguins) +
  scale_colour_manual(values = c(penguin_hues[c(3, 2, 5)]),
                      labels = c("Adelie", "Chinstrap", "Gentoo"))+
  geom_point(aes(x = flipper_length_mm, 
                 y = bill_length_mm,
                 color=species,
                 size = body_mass_g),
             alpha = 0.7)+
  geom_text(data = labels,
            aes(x = species_x, y = species_y, 
                label = species, colour = species),
            family = "Arvo", size = 4.5) +
  labs(x = "Flipper length (mm)\n\n\n",
       y = "Bill length (mm)") +
  scale_size_continuous(name = "Body mass (g)") +
  guides(colour = F, size = F) +
  theme_penguin_light()+labs(x=NULL,y=NULL)


marg <- ggMarginal(p,type = "densigram",groupColour = T,
                   groupFill = T, alpha = 0.6)

bm <- ggplot(penguins,aes(x = body_mass_g, 
                           fill = species, colour = species)) +
  geom_histogram(aes(y = stat(count)),
                 binwidth = 100,
                 alpha = 0.6,
                 show.legend = F) +
  labs(y = "Count",
       x = "Body mass (g)")+
scale_fill_manual(values = c(penguin_hues[c(3, 2, 5)]),
                    labels = c("Adelie", "Chinstrap", "Gentoo")) +
  scale_colour_manual(values = c(penguin_hues[c(3, 2, 5)]),
                      labels = c("Adelie", "Chinstrap", "Gentoo")) +
  theme_penguin_light()+labs(x=NULL,y=NULL)


title <- ggdraw() +
  draw_label("Palmer Penguins",
             colour = penguin_hues[6],
             hjust = .5,vjust = .5,
             size = 20) 


title/marg/bm+plot_layout(ncol = 1,heights = c(0.5,5,1))

