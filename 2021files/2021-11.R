library(tidytuesdayR)
library(tidyverse)
library(patchwork)
library(ggstream)
library(ggbeeswarm)
library(ggtext)



theme_set(theme_void())

tuesdata <- tt_load(2021, week = 11)
readme(tuesdata)



bechdel_df <- tuesdata$movies %>%
  mutate(clean_test = case_when(
    clean_test == "ok" ~ "Pass Bechdel",
    clean_test == "dubious" ~ "Dubious",
    clean_test == "men" ~ "About men",
    clean_test == "notalk" ~ "No talk",
    clean_test == "nowomen" ~ "No women",
  )) %>%
  mutate(
    clean_test = as.factor(clean_test),
    clean_test = fct_relevel(clean_test, c("Pass Bechdel", "Dubious",
                                           "About men", "No talk", "No women"))
  )

perc_rating <- bechdel_df %>%
  count(year, clean_test) %>%
  group_by(year) %>%
  mutate(perc_rating = n / sum(n))



palette <- c("Pass Bechdel" = "#3C989E", "Dubious" = "#F4CDA5",
             "No women" = "#BA415D", "No talk" = "#ED5276","About men" = "#F09CB0")
bck_clr <- "grey30"

bechdel_beeswarm <- bechdel_df %>%
  group_by(year) %>%
  arrange(clean_test) %>%
  mutate(position = row_number()) %>%
  ggplot(aes(year, position, color = clean_test)) +
  geom_beeswarm(size = 0.6) +
  scale_color_manual(values = palette) +
  scale_x_continuous(breaks = seq(1970,2010,10),
                     expand = c(0.01, 0.01)) +
  guides(color = FALSE) +
  theme_void()+
  theme(plot.background = element_rect(fill = bck_clr, color = NA),
        strip.text = element_text(color = "white", margin = margin(5,0,0,0)),
        legend.position = "bottom",
        axis.text.x = element_text(color = "white", size = 9, margin = margin(10,0,0,0)),
        axis.ticks.x = element_line(color = "white"),
        plot.margin = margin(0,0,10,0))


bechdel_beeswarm

bechdel_stream <- perc_rating %>% 
  ggplot(aes(year, perc_rating, fill = clean_test)) +
  geom_stream() +
  scale_fill_manual(values = palette) +
  scale_x_continuous(breaks = seq(1970,2010,10), expand = c(0.01, 0.01)) +
  guides(fill = guide_legend(label.position = "top",
                             title.hjust = 0.5,
                             keywidth = unit(4, "line"),
                             keyheight = unit(1, "line"),
                             nrow = 1
  )
  )+
  theme_void()+
  theme(
    plot.background = element_rect(fill = bck_clr, color = NA),
    strip.text = element_text(color = "white"),
    legend.position = "bottom",
    legend.text = element_text(color = "white", size = 9),
    legend.title = element_blank()
  )

bechdel_stream



bechdel_genre <- bechdel_df %>%
  mutate(main_genre = str_remove(word(genre, 1), ","),
         main_genre = fct_lump_n(main_genre, 7,
         other_level = "Less common\n genres"),
         clean_test = as.factor(clean_test),
         binary = str_to_title(binary)) %>%
  filter(!is.na(main_genre)) %>% 
  count(clean_test, main_genre, binary) %>%
  group_by(main_genre) %>%
  mutate(perc_rating = n / sum(n)) %>%
  ggplot(aes(binary, perc_rating, fill = clean_test)) +
  geom_col() +
  scale_fill_manual(values = palette) +
  facet_wrap(~main_genre, scales = "free_y", ncol = 8) +
  guides(fill = FALSE) +
  theme_void()+
  theme(plot.background = element_rect(fill = bck_clr, color = NA),
        strip.text = element_text(color = "white", margin = margin(0,0,5,0)),
        plot.margin = margin(30,0,0,0),
        panel.spacing.x = unit(1.5, "lines"))

bechdel_genre

final <- bechdel_beeswarm / bechdel_stream / bechdel_genre +
  plot_layout(nrow = 3, heights = c(1, 0.5, 0.2)) +
  plot_annotation(
    title = "Bechdel Test - Presence of women in films",
    theme = theme(
      plot.margin = margin(10,20,10,20),
      plot.background = element_rect(fill = bck_clr, color = NA),
      plot.title = element_text(family = "oswald", size = 20,
                                hjust = 0.5, margin = margin(10,0,0,0), color = "white"),
      plot.subtitle = element_text(family = "roboto",
                                   size = 10, hjust = 0.5,
                                   margin = margin(5,0,0,0),lineheight = 1.1),
      plot.caption = element_text(family = "techmono",
                                  size = 8, color = "white", margin = margin(15,0,0,0))      
    )
  )


final
