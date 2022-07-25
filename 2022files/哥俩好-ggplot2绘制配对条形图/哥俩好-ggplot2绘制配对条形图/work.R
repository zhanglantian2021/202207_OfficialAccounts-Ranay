library(tidyverse)
library(ggtext)

big_dave <- readr::read_csv('big_dave.csv')
times <- readr::read_csv('times.csv')

df1 <- big_dave %>%
  filter(!is.na(definition)) %>%
  mutate(definition = str_to_lower(definition)) %>% 
  count(definition, sort=T) %>% 
  mutate(pct=n/sum(n)) %>%
  mutate(g=1,grp="Big Dave's") %>% slice(1:20)

df2 <- times %>% 
  filter(!is.na(definition)) %>%
  mutate(definition = str_to_lower(definition)) %>% 
  count(definition, sort=T)  %>% 
  mutate(pct=n/sum(n)) %>%
  mutate(g=2,grp="Times") %>% slice(1:20)

df = rbind(df1,df2) %>% group_by(grp) %>%
  mutate(rank=rank(desc(n),ties.method = 'first')) %>%
  ungroup()

selected = df %>% select(definition, g) %>% 
  count(definition) %>%filter(n==2) %>% pull(definition)

df %>% ggplot(aes(x=g, y=rank)) +
  geom_text(aes(label=definition, hjust=ifelse(g==1,1,0))) +
  geom_line(data=df %>% filter(definition %in% selected),aes(group=definition)) +
  geom_segment(data=df %>% filter(g==2),
               aes(x=g+.8, xend=g+.8+pct*600, y=rank, yend=rank, color=pct), size=5) +
  geom_segment(data=df %>% filter(g==1),
               aes(x=g-.8, xend=g-.8-pct*600, y=rank, yend=rank, color=pct), size=5) +
  scico::scale_color_scico(palette="acton", direction=-1) +
  ggnewscale::new_scale_color() +
  geom_text(data=df %>% 
              filter(g==2),aes(x=g+0.85,y=rank,color=I(ifelse(pct>0.0015,"white","black")), 
                               abel=scales::percent(pct, accuracy = .01)),size=3, hjust=0) +
  ggnewscale::new_scale_color() +
  geom_text(data=df %>% filter(g==1),
            aes(x=g-0.85, y=rank, color=I(ifelse(pct>0.0014,"white","black")), 
                label=scales::percent(pct, accuracy = .01)), size=3, hjust=1) +
  annotate(geom="text",y=-.3,x=0.68, label="Big Dave's",size=4.3, fontface="bold") +
  annotate(geom="text",y=-.3,x=2.2, label="Times",size=4.3, fontface="bold") +
  scale_y_reverse() +
  scale_x_continuous(limits=c(-1.5,4.5), expand=c(0,0)) +
  theme_void()+
  theme(legend.position = "none",plot.margin=margin(.1,.1,.1,.1,unit="cm"))


