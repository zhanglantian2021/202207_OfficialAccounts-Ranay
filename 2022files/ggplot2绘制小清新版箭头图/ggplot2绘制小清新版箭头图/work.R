library(tidyverse)
library(magrittr)
library(cowplot)

df <- read_tsv("data.xls")

df %<>% filter(alt!="Total") %>% 
  pivot_longer(-alt) %>%
  filter(alt %in% c("E85","HEVs","PEVs","CNG")) %>%
  group_by(alt) %>%
  mutate(diff = (value/lag(value) - 1)) %>%
  filter(!is.na(diff)) %>%
  mutate(col=case_when(diff<0~"Decrease", diff>0~"Increase"),
         name=parse_number(name))

df %>% ggplot(aes(x=name, y=diff)) +
  geom_hline(yintercept=0, size=.3) +
  geom_segment(aes(y=0, yend=diff, x=name, xend=name, color=col), 
               arrow = arrow(type = "closed", length = unit(1.5, "mm")), 
               size=1, key_glyph = draw_key_rect) +
  scale_y_continuous(breaks=seq(-.5,2,.5), labels=scales::percent_format()) +
  scale_color_manual(values=c("#D68E24","#139E56")) +
  facet_wrap(~alt, scales="free_x") +
  cowplot::theme_minimal_hgrid(11) +
  theme(legend.position="top",
        legend.text=element_text(size=10,color="black"),
        legend.justification = "center",
        panel.spacing.x = unit(1, "lines"),
        panel.spacing.y = unit(1, "lines"),
        legend.title=element_blank(),
        axis.title = element_blank(),
        axis.text=element_text(color="black"),
        strip.text=element_text(size=11.5,color="black",face="bold"),
        plot.margin=margin(.5,.75,.5,.5,unit="cm"),
        plot.background = element_rect(fill="#fafafa", color=NA),
        axis.ticks.length=unit(.25, "cm"),
  ) +guides(color=guide_legend(reverse=T))
