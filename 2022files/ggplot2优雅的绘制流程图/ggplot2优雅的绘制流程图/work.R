library(tidyverse)
library(igraph)
library(showtext)
library(thematic)
# install.packages("thematic")


dat <- tribble(
  ~from, ~to,
  'Are you a horse?', 'No',
  'Are you a horse?', 'Yes',
  'Are you a horse?', 'Maybe',
  'Maybe', 'How many legs\ndo you walk on?',
  'Yes', 'How many legs\ndo you walk on?',
  'No', 'You\'re not a horse',
  'How many legs\ndo you walk on?', 'Two',
  'How many legs\ndo you walk on?', 'Four',
  'Two', 'You\'re not a horse_2',
  'Four', 'Really?',
  'Really?', 'No_2',
  'Really?', 'Yes_2',
  'No_2', 'Can you read\nand write?',
  'Yes_2', 'Can you read\nand write?',
  'Can you read\nand write?', 'Yes_3',
  'Can you read\nand write?', 'No_3',
  'Yes_3', 'You\'re not a horse_3',
  'No_3', 'You\'re reading this,\naren\'t you?',
  'You\'re reading this,\naren\'t you?', 'Yes_4',
  'Yes_4', 'You\'re not a horse_4'
)


#### 创建图形和布局

graph <-  graph_from_data_frame(dat, directed = TRUE)

coords <- graph %>% 
  layout_as_tree() %>% 
  as_tibble(.name_repair = ~c('x', 'y'))

output <- coords %>% 
  mutate(step = vertex_attr(graph, 'name'),label = str_remove(step, '\\_.+'),
         x = -2.5 * x,y = 5 * y,type = case_when(str_detect(label, '\\?') ~ "Question",
      str_detect(step, 'You\'re not a horse') ~ 'Outcome',T ~ 'Answer'))


#### 制作盒子

box_width <- 1.2
box_height <- 1.25
boxes <- output %>%mutate(xmin = x - box_width,xmax = x + box_width,ymin = case_when(
  str_detect(step, '(legs|reading|write)') ~ y - 1.5 * box_height,T ~ y - box_height),
    ymax = case_when(str_detect(step, '(legs|reading|write)') ~ y + 1.5 * box_height,T ~ y + box_height)) 


#### 制作边缘
edges <- dat %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = c("from", "to"),names_to = "s_e",values_to = "step") %>%left_join(boxes, by = "step") %>%
  select(-c(label, type, y, xmin, xmax)) %>%
  mutate(y = ifelse(s_e == "from", ymin, ymax)) %>%
  select(-c(ymin, ymax)) %>% 
  mutate(x = case_when(s_e == 'to' & id %in% c(5, 14) ~ x - box_width,T ~ x))

base_colors <- thematic::okabe_ito(2)


### 数据可视化
ggplot() +geom_path(data = edges, aes(x, y, group = id),arrow = arrow(length = unit(0.25, 'cm'))) +
  geom_rect(data = boxes, aes(xmin = xmin,xmax = xmax, ymin = ymin, ymax = ymax, fill = type)) +
  geom_text(data = boxes,aes(x = x, y = y,label = label),lineheight = 1) +
  theme_void() +
  theme(legend.position = 'none',
        plot.background = element_rect(fill = 'white', colour = NA)) +
  scale_fill_manual(values = c('Question' = base_colors[1],
                               'Answer' = colorspace::lighten(base_colors[1], 0.5),
                               'Outcome' = colorspace::lighten(base_colors[2], 0.1))) +
  coord_cartesian(xlim = c(-4.5,5))

