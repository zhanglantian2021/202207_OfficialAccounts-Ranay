library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)

colors <-c("#E41A1C","#1E90FF","#FF8C00","#4DAF4A","#984EA3",
           "#40E0D0","#FFC0CB","#00BFFF","#FFDEAD","#90EE90",
           "#EE82EE","#00FFFF","#F0A3FF", "#0075DC", 
           "#993F00","#4C005C","#2BCE48","#FFCC99",
           "#808080","#94FFB5","#8F7C00","#9DCC00",
           "#426600","#FF0010","#5EF1F2","#00998F",
           "#740AFF","#990000","#FFFF00")

us417 <- read.csv("US417.csv", header=T)

exports <- us417 %>%
  distinct(source) %>%
  rename(label = source)

imports <- us417 %>%
  distinct(target) %>%
  rename(label = target)

nodes <- full_join(exports, imports,
                   by = "label") %>% 
  mutate(id = 1:nrow(nodes)) %>%
  select(id, everything())


edges <- us417 %>% 
  left_join(nodes, by = c("source" = "label")) %>% 
  rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c("target" = "label")) %>% 
  rename(to = id)


edges <- select(edges, from, to, weight)


net.tidy <- tbl_graph(
  nodes = nodes, edges = edges, directed = TRUE
)


ggraph(net.tidy, layout = "star") + 
  geom_node_point(size=4,aes(color=label),
                  show.legend = F) +
  geom_edge_link(aes(width = weight,
                     color=weight),show.legend = F) + 
  scale_edge_width(range = c(0.5, 2)) +
  geom_node_text(aes(label = label),repel = TRUE) +
  scale_color_manual(values = colors)+
  theme_graph()

