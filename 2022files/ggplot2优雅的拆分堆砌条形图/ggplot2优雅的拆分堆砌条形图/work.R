library(tidyverse)
library(patchwork)

theme_niwot <- function(){
  theme_minimal()+
    theme(axis.text = element_text(color = "black",size = 6),
          strip.text = element_text(color = "black",hjust = 0, 
                                    margin = margin(l = -0.5, b = 5)),
          legend.key.size = unit(0.4, 'cm'),
          legend.text = element_text(color = "black",size =6),
          legend.margin = margin(),
          legend.position = 'top',
          plot.title.position = 'plot',
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major = element_line(size = 0.5,linetype = 3, color = "black"))
}

color_palette <- viridisLite::mako(6)[-c(1, 6)]

mpg_2008 <- mpg %>% 
  filter(year == 2008,!(class %in% c('2seater', 'minivan'))) %>% 
  mutate(class = case_when(
      class %in% c('compact', 'subcompact') ~ '(Sub-)Compact',
      class %in% c('pickup', 'suv') ~ 'Pickup/SUV',
      T ~ str_to_title(class)),
    manufacturer = str_to_title(manufacturer),
    manufacturer = fct_infreq(manufacturer) %>% fct_rev())

unsplit_plot <- mpg_2008 %>% 
  ggplot(aes(y = manufacturer, fill = class)) +
  geom_bar(position = position_stack(reverse = T)) +
  scale_fill_manual(values = color_palette) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_y_discrete(expand = expansion(mult = 0)) +
  geom_vline(xintercept = 0, size = 1) +
  theme_niwot()+
  labs(x = element_blank(),y=element_blank(),fill = element_blank())

class_plots <- mpg_2008 %>% 
  ggplot(aes(y = manufacturer, fill = class)) +
  geom_bar() +
  scale_fill_manual(values =color_palette) +
  facet_wrap(vars(class)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_y_discrete(expand = expansion(mult = 0)) +
  geom_vline(xintercept = 0, size = 1) +
  labs(x = element_blank(),y=element_blank(),fill = element_blank())+
  theme_niwot()

total_plot <- mpg_2008 %>% 
  ggplot(aes(y = manufacturer)) +
  geom_bar(fill = color_palette[4]) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_y_discrete(expand = expansion(mult = 0)) +
  geom_vline(xintercept = 0, size = 1) +
  facet_wrap(vars('Total')) +
  labs(x = element_blank(),y=element_blank(),fill = element_blank())+
  theme_niwot()

split_plot <- wrap_elements(plot = class_plots + total_plot +
    plot_layout(widths = c(0.75, 0.25)))

unsplit_plot / plot_spacer() / split_plot + 
  plot_layout(heights = c(0.425, 0.01,  0.575))

