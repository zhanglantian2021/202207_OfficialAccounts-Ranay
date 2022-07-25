library(tidyverse)
library(tidytuesdayR)
library(tidytext)
library(ggtext)
library(lubridate)

theme_set(theme_light())



un_votes <- read.csv("unvotes.csv",,sep=",")
issues <- read.csv("issues.csv",sep=",")
roll_calls <- read.csv("roll_calls.csv",sep=",")
roll_calls <- roll_calls %>% 
  mutate(year = year(date)) %>%
  left_join(issues)

roll_calls %>%
  group_by(year) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = year, y = n)) +
  geom_line()

un_plot <- roll_calls %>%
  filter(!is.na(issue)) %>%
  count(year, issue, sort = T) %>%
  arrange(year) %>%
  ggplot(aes(x = year, y = n)) +
  geom_col(aes(fill = issue)) +
  geom_line(aes(x = year, y = n),
            color = "black",data = roll_calls %>% 
              count(year)) +
  ggthemes::theme_wsj() +
  ggthemes::scale_fill_ptol()+
  theme(legend.title = element_blank())


ggsave("un_plot.jpg", 
       un_plot, 
       height = 8, width = 12, 
       units = "in", dpi = 300)


colors <-c("#E41A1C","#1E90FF","#FF8C00","#4DAF4A","#984EA3",
           "#40E0D0","#FFC0CB","#00BFFF","#FFDEAD","#90EE90",
           "#EE82EE","#00FFFF","#F0A3FF", "#0075DC", 
           "#993F00","#4C005C","#2BCE48","#FFCC99",
           "#808080","#94FFB5","#8F7C00","#9DCC00",
           "#C20088","#003380","#FFA405","#FFA8BB",
           "#426600","#FF0010","#5EF1F2","#00998F",
           "#740AFF","#990000","#FFFF00")

bar <- roll_calls %>%
  filter(!is.na(issue)) %>%
  count(year, issue, sort = T) %>%
  arrange(year) %>%
  ggplot(aes(x = year, y = n)) +
  geom_col(aes(fill = issue))+
  scale_fill_manual(values = colors)+labs(fill="")+
  labs(x=NULL,y=NULL)+
  theme(legend.position = "top")

ggsave("bar.jpg", 
       bar, 
       height = 8, width = 12, 
       units = "in", dpi = 300)
  
