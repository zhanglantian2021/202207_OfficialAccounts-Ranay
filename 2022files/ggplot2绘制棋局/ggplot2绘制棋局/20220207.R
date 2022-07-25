library(tidyverse)

ratings <- read_csv("ratings.csv")
details <- read_csv("details.csv")

ratings %>% inner_join(details, by='id') %>% 
  mutate(category:=lapply(lapply(str_split(boardgamecategory, ','),
                                 str_trim),str_replace_all, '[[:punct:]]','')) %>%
  unnest(category) %>%
  mutate(mechanic:=lapply(lapply(str_split(boardgamemechanic, ','),
                                 str_trim),str_replace_all, '[[:punct:]]','')) %>%
  unnest(mechanic) -> df_full_unnested

details %>%
  mutate(category:=lapply(lapply(str_split(boardgamecategory,','),
                                 str_trim), str_replace_all,'[[:punct:]]','')) %>%
  unnest(category) -> df_category

df_category %>%
  count(category) %>%
  arrange(desc(n)) %>%
  top_n(19) %>%
  pull(category) -> top_categories

details %>%
  mutate(mechanic:=lapply(lapply(str_split(boardgamemechanic,','),
                                 str_trim),str_replace_all,'[[:punct:]]','')) %>%
  unnest(mechanic) -> df_mechanic

df_mechanic %>%
  count(mechanic) %>%
  arrange(desc(n)) %>%
  filter(!is.na(mechanic)) %>%
  top_n(19) %>%
  pull(mechanic) -> top_mechanics


df_full_unnested %>%
  filter(category %in% top_categories, mechanic %in% top_mechanics) %>%
  group_by(category, mechanic) %>%
  summarise(avg_rating := mean(average)) %>%
  mutate(global_mean:=mean(avg_rating)) %>%
  mutate(rating_level:=ifelse(avg_rating>=global_mean,'over', 'under')) %>%
  ggplot() +
  geom_point(aes(category, reorder(mechanic, avg_rating), color=rating_level), size=5) +
  scale_color_manual(values=c('black','white')) +
  coord_equal() +
  theme_minimal() +
  theme(text=element_text(family='Cinzel', color='#e5e5e5'),
        panel.grid=element_line(color='black', size=0.5),
        panel.background=element_rect(fill='#b58225', color='#b58225'),
        legend.position = 'none',
        plot.margin=margin(1,2,1,0,'cm'),
        axis.text.x = element_text(angle=45, hjust=1),
        axis.title=element_blank(),
        axis.text=element_text(color='black', size=8))
