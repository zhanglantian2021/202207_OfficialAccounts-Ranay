library(tidyverse)
library(lubridate)
library(patchwork)

colors <- c("#3B9AB2", "#78B7C5", "#EBCC2A","#E1AF00","#F21A00",
           "#40E0D0","#FFC0CB","#00BFFF","#FFDEAD","#90EE90",
           "#EE82EE","#00FFFF","#F0A3FF", "#0075DC", 
           "#993F00","#4C005C","#2BCE48","#FFCC99",
           "#808080","#94FFB5","#8F7C00","#9DCC00",
           "#C20088","#003380","#FFA405","#FFA8BB",
           "#426600","#FF0010","#5EF1F2","#00998F",
           "#740AFF","#990000","#FFFF00",
           "#85D4E3","#F4B5BD","#9C964A","#CDC08C", "#FAD77B",
           "#00A08A","#F2AD00","#F98400","#5BBCD6",
           "#1E90FF","#FF8C00")


cn = 1.5

tweets_w1_raw <- read_csv("tweets_w1.csv")

tweets <- tweets_w1_raw %>% 
  mutate(
    h = hour(created_at),
    m = minute(created_at)
  ) %>% 
  count(h, m, source) %>% 
  mutate(source=as.factor(source))

tweets





barplot <- ggplot(tweets,aes(m,h,fill=source))+
  geom_bar(stat="identity")+
  scale_fill_manual(values = colors)+
  theme(legend.title=element_blank())+
  scale_y_continuous(expand=c(0,0))+
  theme_minimal()+
  theme(legend.position = "non")+
  labs(
    x = "minute",
    y = "hour"
  )

barplot

heatmap <- ggplot(tweets) +
  geom_tile(aes(m, h, width = n/cn, height = n/cn,
                fill = source),alpha=0.6)+
  scale_x_continuous(breaks = seq(0, 60, by = 10), minor_breaks = 0:60) +
  scale_y_reverse(limits = c(23, 0), breaks = seq(20, 0, by = -4),
                  minor_breaks = 0:23)+
  coord_cartesian(expand = FALSE) +
  scale_fill_manual(values = colors)+
  labs(
    fill = "",
    x = "minute",
    y = "hour"
  ) +
  labs(x=NULL,y=NULL)+
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 9),
    legend.key.size = unit(0.75, "line"),
    legend.title = element_text(angle = 90, hjust = 0.5),
    plot.margin = margin(10,10,10,10))


heatmap


