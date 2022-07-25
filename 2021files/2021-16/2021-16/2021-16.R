library(tidyverse)
library(patchwork)
library(showtext)
library(ggfx)

post_trend <- read.delim("post_trend.xls",header = T,check.names = F,sep="\t")

post_trend
years <- tibble(year = c(seq(1780, 1885, 15), seq(1915, 2020, 15)),
                y = c(rep(-150, 8), rep(150,8)))

blue <- "#1a7bc5"
red <- "#f1434a"
darkred <- "#8d0613"
darkblue <- "#105182"

main <- ggplot() +
  as_group(
    geom_col(data = post_trend, aes(year, nb_established),
             fill = "#C4E4FA", width = 1),
    geom_col(data = post_trend, aes(year, -nb_discontinued),
             fill = "#F4DCDD", width = 1),
    geom_col(data = filter(post_trend, gap > 0),
             aes(year, gap), width = 1, fill = blue),
    geom_col(data = filter(post_trend, gap < 0),
             aes(year, gap), width = 1, fill = red),
    id = "group"
  ) +
  with_blend(
    geom_text(data = years, aes(x = year,
                                y = y,label = year),
              color = "white",
              family = "roboto condensed",fontface = "bold"),
    bg_layer = "group",
    blend_type = "xor"
  )+
  annotate("text", x = 1900, y = 0, label = "1900", angle = 45,
           color = "grey50", family = "roboto condensed",
           fontface = "bold", size = 5)+
  geom_step(data = post_trend,aes(year-0.5,gap),color = "grey40")+
  theme_void() +
  theme(plot.background = element_rect(fill = "grey50", color = NA),
        plot.margin = margin(10, 10, 50, 10))

main



lg1 <- ggplot() +
  geom_col(data = post_trend, aes(year,nb_established),
           fill = "#C4E4FA", width = 1)+
  geom_col(data = post_trend, aes(year,-nb_discontinued),
           fill = "#F4DCDD", width = 1) +
  annotate("text", x = 1925, y = 2000,
           label = "Established",
           size = 2, color = "white",
           hjust = 0, family = "roboto condensed") +
  annotate("text", x = 1925, y = -2400,
           label = "Discontinued",
           size = 2, color = "white",
           hjust = 0, family = "roboto condensed") +
  scale_x_continuous(expand = c(0,0.5)) +
  theme_void() +
  theme(plot.background = element_rect(fill = "grey50",color = NA))

lg1

lg2 <- ggplot() +
  geom_col(data = filter(post_trend, gap > 0),
           aes(year, gap), width = 1, fill = blue) +
  geom_col(data = filter(post_trend, gap < 0),
           aes(year, gap), width = 1, fill = red) +
  geom_step(data = post_trend, aes(year-0.5,gap),
            color = "grey40", size = 0.2) +
  annotate("text", x = 1800, y = 2100, label = "Net increase",
           size = 2, color = "white", hjust = 0,
           family = "roboto condensed") +
  annotate("text", x = 1925, y = -2000,
           label = "Net decrease", size = 2,
           color = "white", hjust = 0, family = "roboto condensed") +
  scale_x_continuous(expand = c(0,0.5)) +
  theme_void() +
  theme(plot.background = element_rect(fill = "grey50", color = NA))


lg2

final <- main +
  inset_element(lg1, 0.65,0.05,0.95,0.35,
                align_to = "full", clip = FALSE) +
  inset_element(lg2, 0.05,0.05,0.35,0.35,
                align_to = "full", clip = FALSE) +
  plot_annotation(
    title = "Establishment and discontinuation of US post offices since 1780",
    theme = theme(
      plot.background = element_rect(fill = "grey50", color = NA),
      plot.title = element_text(family = "oswald", size = 10, color = "white",
                                hjust = 0.5, face = "bold",
                                margin = margin(10,0,0,0))
    )
  )


final

