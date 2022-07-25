library(tidyverse)
library(ggtext)

df <- read_csv('hotels.csv') %>% 
  rename(yr = arrival_date_year, mo = arrival_date_month) %>% 
  mutate(mo = str_sub(mo, end = 3), mo = factor(mo, levels = month.abb)) %>% 
  group_by(hotel, yr, mo) %>% 
  summarise(M_ADR = mean(adr)) %>% 
  pivot_wider(names_from = yr, values_from = M_ADR)


df %>% view()
  
  
ggplot(df, aes(mo)) +
  facet_wrap(~ hotel, strip.position = 'bottom') +
  geom_bar(aes(y=`2017`,fill='2017'),stat='identity',width=.2) +
  geom_bar(aes(y=`2016`,fill='2016'),stat='identity',width=.4) +
  geom_bar(aes(y=`2015`,fill='2015'),stat='identity',width=.6) +
  scale_fill_manual('',values=c('grey50','grey65','grey80')) +
  scale_y_continuous(expand = expansion(c(0, .05))) +
  expand_limits(y = 0) +
  labs(x = '', y = 'Average Daily Rate') +
  cowplot::theme_minimal_hgrid() +
  theme(axis.line.x = element_line(color = 'grey80'), axis.ticks = element_blank(),
        axis.title.y = element_text(size = 12, face = 'bold', margin = margin(r = 10)),
        legend.position = 'bottom',
        strip.placement = 'outside', strip.text = element_text(size = 12, face = 'bold'),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), 'cm'))
