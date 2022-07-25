library(tidyverse)

df_agrupado <- read_csv("vinos.csv") %>% 
  mutate(grupo:=c('Tinto', 'Blanco', 'Espumoso','Rosado','Otros', 'Espumoso', rep('Otros',4))) %>%
  group_by(grupo) %>% 
  summarise(hl:=sum(hl)) %>%
  mutate(pct_hl:=round(hl/sum(hl)*100,1)) %>%
  arrange(desc(pct_hl))

colors <- c("#7C7C7C", "#A06177", "#AF6458", '#D9AF6B', "#855C75")

ggplot(df_agrupado, aes(x=1, y=pct_hl, fill=reorder(grupo, pct_hl))) +
  geom_col(position=position_stack()) +
  geom_text(position=position_stack(vjust=0.5), 
            aes(x=2, y=pct_hl, label=paste0(grupo, '\n(', pct_hl, '%)'), 
                color=reorder(grupo, pct_hl)), family='mono', size=4) +
  scale_fill_manual(values=colors) +
  scale_color_manual(values=colors)


last_plot() +
  coord_polar(theta="y") 


last_plot() +
  xlim(c(-1, 2)) +
  theme_void() +
  theme(legend.position = "none",
        text = element_text(family='mono', color='white'),
        plot.title = element_text(family='Roboto-Thin', size=20, hjust=0.5),
        plot.margin = unit(c(1,2,1,2), 'cm'),
        plot.background = element_rect(fill='#212020'))

