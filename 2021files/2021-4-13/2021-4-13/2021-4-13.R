library(tidyverse)
library(lubridate)
library(ggalluvial)
library(igraph)
library(colormap)
library(ggraph)
library(ggtext)
library(RColorBrewer)
library(jkmisc)
library(ggfx)
library(glue)


my_df <- read.delim("my_df.xls",sep="\t",check.names = F)

final_plot = my_df %>%
  ggplot(aes(axis1 = regions, axis2 = category, y = values)) +
  scale_x_discrete(limits = c("regions", "category"), expand = c(.2, .05)) +
  geom_alluvium(aes(fill = regions)) +
  scale_fill_brewer(palette="Paired")+
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),
            size = 5) +
  theme_minimal() +
  theme_void()+
  theme(legend.position = "None")

final_plot

####----------------------------
hfx_temp <- read_csv("mm8202251.txt") %>%
  slice(-1) %>%
  select(-starts_with("X")) %>%
  select(Year, matches(month.abb), Annual) %>%
  mutate(across(everything(), ~replace(.x, .x == -9999.9, NA))) %>%
  mutate(across(everything(), as.numeric)) %>%
  mutate(anom = Annual - mean(Annual, na.rm = TRUE))


plot <- ggplot()+
  as_reference(geom_text(aes(x = 1868, y = 1, label = "HALIFAX"), family = "Passion One",
                         size = 100, hjust = 0, color = "white"),
               id = 'text') +
  with_blend(geom_tile(data = hfx_temp, aes(x = Year, y = 1, fill = Annual)),
             bg_layer = "text",
             blend_type = "in",
             id = "blended") +
  with_shadow("blended", sigma = 5) +
  scale_fill_distiller("Temperature Anomaly", palette = "RdBu") +
  labs(x = NULL,
       y = NULL) +
  theme_jk(grid = FALSE,
           plot_title_family = "Oswald Light") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_markdown(),
        legend.position = "none") +
  guides(fill = guide_colorbar(title.position = "top"))


plot

#-----------------------------------------------------

library(extrafont)
loadfonts(device="win")

oignons <- read.csv(paste0("oignons.txt"))

oignons %>% 
  ggplot()+
  with_shadow(
    geom_violin(aes(X50,y=1),fill="sandybrown",color=NA),
    colour="gray60")+
  scale_size_continuous(range = c(1,75))+
  coord_flip()+
  theme_void()+
  theme(text=element_text(family="Courier New"),
        plot.title = element_text(hjust=0.5,size=30,
                                  face="bold"),
        plot.background = element_rect(color="beige",fill="beige")
  )

