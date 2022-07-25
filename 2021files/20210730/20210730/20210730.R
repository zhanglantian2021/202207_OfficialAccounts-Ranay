library(tidyverse)
library(ggpubr)

olympics <- readr::read_csv('olympics.csv')

glimpse(olympics)

col2 <-c("#FF0000","#F98400","#5BBCD6")

dat <- olympics %>%
  filter(sport == "Archery") %>%
  group_by(team) %>%
  count(medal) %>%
  mutate(freq = n / sum(n)*100) 

p <- dat %>%
  filter(team == "South Korea") %>%
  drop_na() %>% 
  ggdonutchart("freq",label = "medal", fill="medal") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(family="Lato",hjust=0.5, vjust=-48, size=14),
        plot.margin = margin(-3, -3, -3, -3, "cm")) + 
  xlim(c(-2, 4)) + 
  scale_fill_manual(values = col2) +
  ggtitle("South Korea")

p2 <- dat %>%
  filter(team == "Belgium") %>%
  drop_na() %>% 
  ggdonutchart("freq", label = "medal", fill="medal") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(family="Lato", hjust=0.5, vjust=-48, size=14),
        plot.margin = margin(-3, -3, -3, -3, "cm")) +
  xlim(c(-2, 4)) +
  scale_fill_manual(values = col2) +
  ggtitle("Belgium")

p3 <- dat %>%
  filter(team == "France") %>%
  drop_na() %>% 
  ggdonutchart("freq", label = "medal", fill="medal") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(family="Lato", hjust=0.5, vjust=-48, size=14),
        plot.margin = margin(-3, -3, -3, -3, "cm")) +
  xlim(c(-2, 4)) +
  scale_fill_manual(values = col2) +
  ggtitle("France")

p4 <- dat %>%
  filter(team == "United States") %>%
  drop_na() %>% 
  ggdonutchart("freq", label = "medal", fill="medal") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(family="Lato", hjust=0.5, vjust=-107,size=14),
        plot.margin = margin(-7, -3.5, 0, 0, "cm")) +
  xlim(c(-2, 4)) +
  scale_fill_manual(values = col2) +
  ggtitle("USA")

p5 <- dat %>%
  filter(team == "China") %>%
  drop_na() %>% 
  ggdonutchart("freq", label = "medal", fill="medal") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(family="Lato", hjust=0.5, vjust=-107,size=14),
        plot.margin = margin(-7, 0, 0, -3.5, "cm")) +
  xlim(c(-2, 4)) + 
  scale_fill_manual(values = col2) +
  ggtitle("China")

a1 <- ggarrange(p, p2, p3, ncol = 3)
a2 <- ggarrange(p4, p5)

ggarrange(a1, a2, ncol = 1,nrow = 2) %>% 
  annotate_figure(top = text_grob("1920-2016 年奥运会射箭奖牌",
                                  x=0.5, y=-16, family="Lato Semibold", size = 20))