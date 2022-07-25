library(tidyverse)
library(scales)
library(ggtext)


binded_first <- read.delim("data.xls",sep="\t")

facet_labels <- data.frame(test = c("aca199", "ca125", "cea", "crp"), 
                           label = c("CA 19-9 [U/mL]", "CA 125 [U/mL]",
                                     "CEA [ng/mL]", "CRP [mg/mL]"))

my_y_labels <- c(0,.01,.1,.5,1,5,10,50,100,500,"1,000","10,000")

stats_data_first <- binded_first %>% 
  pivot_longer(first_aca199:first_crp,names_to = "names",
               values_to = "values") %>% 
  separate(names, into = c("time", "test"), sep = "_") %>% 
  group_by(test) %>% 
  summarise(p_value = round(wilcox.test(values~group)$p.value, 2)) %>% 
  mutate(label = if_else(p_value < .05 , paste0(p_value, "*"), as.character(p_value)),
         label_asteriks = if_else(p_value == 0, "p<0.01*", paste0("p=", as.character(label))))

binded_first %>% 
  pivot_longer(first_aca199:first_crp,
               names_to = "names",
               values_to = "values") %>% 
  separate(names, into = c("time", "test"), sep = "_") %>% 
  ggplot(aes(group, values)) + 
  stat_boxplot(geom ="errorbar", width = .3, size=.7,  coef = 1) +
  geom_boxplot(width = .55, outlier.shape = NA,  coef = 1) +
  geom_point(position = position_jitter(seed = 2021, width = .25),
             aes(color = group, shape = group), alpha = .4, size = 3) +
  scale_y_continuous(trans = "log10", breaks = c(0, .01, .1, .5, 1, 5, 10, 50, 100, 500, 1000, 10000), 
                     limits = c(0.003,9500), 
                     labels = as.character(my_y_labels)) +
  scale_color_manual(values =  c("#3B9AB2", "#00A08A")) +
  facet_wrap(~ test,  nrow = 1, strip.position = "right") +
  scale_x_discrete(position = "top",labels = c("Controls", "iCRS/HIPEC")) +
  theme_bw()+
  theme(strip.text = element_blank(),
        panel.spacing = unit(0, "cm"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 10, vjust = 1,color="black"),
        axis.ticks.x.top = element_line(color = "black", size = .5),
        axis.ticks.length = unit(-0.2, "cm"),
        axis.ticks.y = element_line(color = "black", size = .5),
        axis.text.y = element_text(size = 10,color="black"),
        legend.position = "none",
        panel.grid = element_blank(),
        plot.margin=unit(c(0.5,0.5,0.5,0.1),"cm")) +
  geom_rect(xmin=1, xmax=2, ymin=-1.7, ymax=-2.3, color="black", fill="white", size = .3)+
  geom_text(data = facet_labels, aes(x = 1.5, y = .01, label = label), size =3.2) +
  geom_text(data = stats_data_first, aes(x = 1.5, y = 3900, label = label_asteriks), size = 5) +
  geom_segment(aes(x=1, xend=2, y=2300, yend=2300), color="black") +
  geom_segment(aes(x=1, xend=1, y=1600, yend=3500), color="black") +
  geom_segment(aes(x=2, xend=2, y=1600, yend=3500), color="black") 
