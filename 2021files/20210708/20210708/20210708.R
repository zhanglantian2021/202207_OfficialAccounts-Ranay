library(tidyverse)
library(ggforce)

tibble(sides=c(3,4,5,6,7,8,9),
      x=c(rep(1,7)),
      y=c(1:7)) %>% 
  ggplot()+
  geom_regon(aes(x0=x,y0=y,sides=sides,
                 angle=0,r=0.25,
                 fill=sides %>% as.character))+
  scale_fill_brewer(name="Rating")+
  coord_fixed()+
  theme_void()



library(tidyverse)
library(forcats)
library(gapminder)
library(ggthemes)
library(wesanderson)

holidays <- readr::read_csv('holidays.csv') %>%
  select(-event_commemorated_and_notes) %>%
  mutate(independence_from = case_when(independence_from ==
                                         "Russian Soviet Federative Socialist Republic" ~ "Soviet Union",
    independence_from =="Russian Soviet Federative Socialist Republic and German Empire" ~ "Soviet Union",
    independence_from =="Spanish Empire" ~ "Spain",
    independence_from =="United Kingdom of Great Britain" ~ "United Kingdom",
    independence_from =="Kingdom of Great Britain" ~ "United Kingdom",
    independence_from =="United Kingdom of Great Britain and Ireland" ~ "United Kingdom",
    independence_from =="United Kingdom of Portugal, Brazil and the Algarves" ~ "Portugal",
    independence_from =="United Kingdom and the British Mandate for Palestine" ~ "United Kingdom",
    independence_from =="SFR Yugoslavia" ~ "Yugoslavia",
    independence_from =="Socialist Federal Republic of Yugoslavia" ~"Yugoslavia",
    independence_from =="Empire of Japan and France" ~ "Empire of Japan",
    independence_from == "Spanish Empire[72]" ~ "Spain")) %>% 
  mutate(independence_from = 
           recode_factor(independence_from, "Soviet Union[80]" = "Soviet Union")) %>%
  mutate(independence_from = 
           recode_factor(independence_from, "Soviet Union[55]" = "Soviet Union")) %>%
  mutate(across(independence_from,fct_explicit_na, "other"))
  
round_to_decade = function(value){ return(round(value / 10) * 10) }
holidays <- holidays %>%
  mutate(decade = round_to_decade(year))



mapdata <- map_data("world") %>%
  filter(!(region=="Antarctica")) %>%
  filter(!(region=="Greenland")) %>%
  filter(!(region=="French Southern and Antarctic Lands")) %>% 
  mutate(region = recode(region,
                         USA="United States",
                         UK="United Kingdom"))

holidays %>% 
  mutate(independence_from = fct_lump(independence_from, n=7)) %>%
  ggplot() + 
  geom_map(dat = mapdata, map = mapdata,
           aes(map_id=region),fill="#E86F00", color=NA) +
  geom_map(aes(map_id=country, fill=independence_from), map=mapdata) + 
  expand_limits(x = mapdata$long, y = mapdata$lat) +
  coord_map(projection = "gilbert", xlim = c(-180, 180)) +  
  ggthemes::theme_map() +
  theme(
    text=element_text(family="Garamond"),   
    plot.title = element_text(size=22, hjust = 0.5),
    plot.background = element_rect(fill = "white", color=NA),
    legend.position = "top",
    legend.justification = "center",
    legend.background=element_blank(),
    legend.key = element_rect(colour = NA, fill = NA), 
    legend.box.background = element_blank(),
    legend.text=element_text(size=rel(2))) +
  scale_fill_manual(values = wes_palette("Zissou1", 8, type = "continuous"),
                    na.value="#E86F00", na.translate=F) +
  guides(fill=guide_legend(title="", ncol=3)) +
  theme(plot.caption= element_text(size=16, hjust=1))
