library(tidyverse)
library(countrycode)
library(janitor)
library(RColorBrewer)
library(scales)
library(patchwork)

production <- readr::read_csv('seafood-and-fish-production-thousand-tonnes.txt')
production <-janitor::clean_names(production)

production$entity <- as.factor(production$entity)

bigproducer <- production %>%
  filter(entity %in% c("Bangladesh","China","Egypt","India","Indonesia","Philippines","Norway","Japan","South Korea","Vietnam","World"))

p1 = ggplot(bigproducer) +
  geom_area(aes(x = year, y = commodity_balances_livestock_and_fish_primary_equivalent_marine_fish_other_2764_production_5510_tonnes,fill = entity))+
  scale_fill_brewer(palette = "Paired")+
  scale_y_continuous(labels = comma)+
  ylab("tonnes per year")+
  labs(title = "Marine fish production")+
  theme(plot.title = element_text(color = "#3F5079", hjust = 0.5))+
  theme(legend.title = element_blank())+
  theme(plot.background = element_rect(fill = "white"))+
  theme(panel.background = element_rect(fill = "#F9E8D9",color = "#FFF7EB"))


p2 = ggplot(bigproducer) +
  geom_area(aes(x = year, y = commodity_balances_livestock_and_fish_primary_equivalent_freshwater_fish_2761_production_5510_tonnes, fill = entity))+
  scale_fill_brewer(palette = "Paired")+
  scale_y_continuous(labels = comma)+
  ylab("tonnes per year")+
  labs(title = "Freshwater fish production")+
  theme(plot.title = element_text(color = "#3F5079", hjust = 0.5))+
  theme(legend.title = element_blank())+
  theme(plot.background = element_rect(fill = "white"))+
  theme(panel.background = element_rect(fill = "#F9E8D9",color = "#FFF7EB"))


p1 + p2 + plot_layout(ncol = 1, nrow = 2, guides = "collect")+
  plot_annotation(title="Production by leading seafood producers")

ggsave("seafoodproduction.png",width = 297,height = 210,units = c("mm"),dpi = 300)

