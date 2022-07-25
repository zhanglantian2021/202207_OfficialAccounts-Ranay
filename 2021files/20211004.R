library(tidyverse)
library(gt)

iris %>% head(10) %>% gt() %>% 
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_column_labels(everything())) %>% 
  tab_spanner(label=md("**<span style = 'color:#0072B2;'>Sepal</span><sup>[1]</sup>**"),
              columns=1:2) %>% 
  tab_spanner(label=md("**Petal.Length**"),columns=3:4) %>%
  cols_label(Sepal.Length= md("Sepal.Length<sup>[1]</sup>"),
             Species = md("Species<br> ")) %>% 
  fmt_number(columns=1:4,decimals= 2) %>% 
  cols_align(align = "center",columns = 1:5) %>% 
  cols_width(everything() ~ px(120)) %>% 
  tab_style(style = list(cell_text(color = "white"),
            cell_fill(color = scales::alpha("#1E90FF",0.7))),
            locations = list(cells_body(columns = 2,rows = `Sepal.Width` < 3),
                             cells_body(columns = 3,rows = `Petal.Width` < 0.4))) %>% 
  tab_header(title = md("**iris**"),subtitle = md("**R优雅的绘制表格**")) %>% 
  tab_options(
    column_labels.border.top.color = "grey",
    column_labels.border.top.width = px(2.5),
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width=px(2.5),
    table_body.hlines.color = "black",
    table_body.hlines.width=px(1),
    table.border.bottom.color = "grey",
    table.border.bottom.width = px(2.5),
    data_row.padding = px(10)) %>% 
  tab_footnote(
    "Data: iris 2021-10-04",
    locations = cells_column_labels(5)) %>% 
  tab_source_note(md("**Table: R语言分析指南**"))

