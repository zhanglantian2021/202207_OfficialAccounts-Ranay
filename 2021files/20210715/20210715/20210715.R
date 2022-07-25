library(tidyverse)
library(ggtext)
library(circlize)

netflix <- read_csv("netflix_titles.csv")

netflix_lgbtq <- netflix %>%
  filter(str_detect(listed_in,"LGBTQ Movies")) %>%
  separate(listed_in,into = c("genre1", "genre2", "genre3"),
           sep = ", ",fill = "right") %>%
  separate(date_added, into = c(NA, "added_year"), sep = ", ") %>%
  mutate(country = if_else(str_detect(title, "Wish You"),
                           "South Korea", country), 
         added_year = as.numeric(added_year)) %>%
  pivot_longer(genre1:genre3, names_to = "genre_num", values_to = "genre")


chord_lgbtq <- netflix_lgbtq %>% 
  filter(!is.na(genre) & genre != "LGBTQ Movies") %>%
  mutate(genre = factor(genre),
         added_year = as_factor(added_year)) %>% 
  group_by(genre) %>%
  mutate(count_genre = n()) %>%
  filter(count_genre > 1) %>% 
  ungroup() %>% 
  group_by(genre, added_year) %>% 
  count() %>% 
  ungroup()

chord_lgbtq


grid.col = c(`Comedies` = "#FF69B6",   
             `Cult Movies` = "#FF0018",
             `Documentaries` = "#FFA52C", 
             `Dramas` = "green", 
             `Independent Movies` = "#008018",
             `International Movies` = "#00C0C0", 
             `Music & Musicals` = "#400098", 
             `Romantic Movies` = "#86007D",
             `2015` = "green",`2016` = "green",
             `2017` = "green", `2018` = "green", 
             `2019` = "green", `2020` = "green",
             `2021` = "green")

circos.par(canvas.xlim=c(-1,1),canvas.ylim=c(-1,1),start.degree = 0)
chordDiagram(chord_lgbtq,
             order = c("Romantic Movies","Music & Musicals",
                       "International Movies", 
                       "Independent Movies", "Dramas",
                       "Documentaries", "Cult Movies",
                       "Comedies", "2015", "2016", "2017",
                       "2018", "2019", "2020", "2021"),
             link.sort = FALSE, 
             link.decreasing = TRUE, 
             grid.col = grid.col, 
             transparency = 0.1, 
             annotationTrack = "grid",
             preAllocateTracks = list(track.height = .1))

for(si in get.all.sector.index()) {
  myCol <- grid.col[si]
  xlim = get.cell.meta.data("xlim",sector.index = si,track.index = 1)
  ylim = get.cell.meta.data("ylim",sector.index = si,track.index = 1)
  circos.text(mean(xlim), ylim[1],labels = si,sector.index = si,
              track.index = 1, 
              facing = "clockwise", 
              col = myCol,
              cex=0.8,
              adj=c(0,.5),
              niceFacing = T)
}

circos.clear()


