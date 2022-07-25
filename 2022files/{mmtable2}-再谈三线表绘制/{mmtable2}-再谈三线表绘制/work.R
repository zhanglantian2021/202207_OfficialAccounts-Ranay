devtools::install_github("ianmoran11/mmtable2")

library(gapminder)
library(tidyverse)
library(stringr)
library(gt)
library(mmtable2)

# 整合数据
gm_df <- gapminder_mm %>% filter(var != "Life expectancy")
style_list <- list(cell_borders(sides = "top",color = "grey"))

# 案例一
gm_df %>% 
  mmtable(cells = value) +
  header_left(year) +
  header_top(country) +
  header_left_top(var)  +
  header_top_left(continent) + 
  header_format(var, scope = "table", style = style_list)

# 案例二格式化表格
gapminder_mm %>% 
  filter(var != "Life expectancy") %>% 
  mmtable(cells = value) +
  header_top(year) +
  header_left(country) +
  header_top_left(var)  +
  header_left_top(continent)  +
  cells_format(cell_predicate = T, style = list(cell_text(align = "right"))) +
  header_format(header = year, style = list(cell_text(align = "right"))) +
  header_format("all_cols", style = list(cell_text(weight = "bolder"))) +
  header_format("all_rows", style = list(cell_text(weight = "bolder"))) +
  header_format(continent, scope= "table", 
                style = list(cell_borders(sides = "top",color = "grey"))) 

# 案例三 通过 %>%连接数据
gapminder_mm %>% 
  filter(var != "Life expectancy") %>% 
  mmtable(cells = value, use_default_formats = T) %>% 
  add_header_top(year) %>% 
  add_header_left(country) %>% 
  add_header_top_left(var)  %>% 
  add_header_left_top(continent)  %>% 
  add_cells_format(cell_predicate = T,style = list(cell_text(align = "right",color="white"),
                                                    cell_fill(color="grey80"))) %>% 
  add_header_format(header = year,style =list(gt::cell_text(align = "right",color="black"),
                                                gt::cell_fill(color = "lightcyan"))) %>% 
  add_header_format("all_cols", style = list(cell_text(weight = "bolder"))) %>% 
  add_header_format("all_rows", style = list(cell_text(weight = "bolder"))) %>% 
  cols_align(align="center",columns=1:2) %>% 
  tab_options(table_body.hlines.color ="grey80",
              table_body.hlines.width =px(2)
              ) %>% 
  add_header_format(continent,scope= "table", 
                    style = list(cell_borders(sides = c("top","bottom"),
                                              color="white",weight = px(2))
                                 ))




