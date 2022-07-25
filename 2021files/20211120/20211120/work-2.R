library(tidyverse)
library(extrafont)
library(ggtext)
library(ggsci)

chain_investment <- read_csv('chain_investment.txt') 

df %>% as_tibble()

df1 <- chain_investment %>%
  filter(meta_cat == "Social" & 
           category %in% c("Education", "Health", "Public safety")) %>%
  group_by(year) %>%
  mutate(total = sum(gross_inv_chain)) %>%
  ungroup() %>%
  mutate(prop = gross_inv_chain / total,
         category = factor(category,
                           levels = c("Public safety", "Health", "Education")))

df2 <- chain_investment %>%
  filter(meta_cat == "Education") %>%
  mutate(category = ifelse(grepl("S&L", category), "State and Local Government", category)) %>%
  group_by(year, category) %>%
  summarise(gross_inv_chain = sum(gross_inv_chain),
            .groups = "drop") %>%group_by(year) %>%
  mutate(total = sum(gross_inv_chain)) %>%
  ungroup() %>%
  mutate(prop = gross_inv_chain / total)

df3 <- chain_investment %>%
  filter(grepl("Total", category) &
           meta_cat == "Total infrastructure") %>%
  group_by(year) %>%
  mutate(total = sum(gross_inv_chain)) %>%
  ungroup() %>%
  mutate(prop = gross_inv_chain / total)

df <- df1 %>% bind_rows(df2) %>% bind_rows(df3) %>% 
  mutate(years=year) %>% 
  unite(.,col="type",meta_cat,years,sep="-",remove = T,na.rm = F) %>%
  mutate(type=str_remove(type,"NA-"))

theme_set(theme_minimal(base_size = 12))

theme_update(
  panel.grid.minor=element_blank(),
  panel.grid.major=element_blank(),
  panel.background=element_rect(fill = bcolor,color = NA),
  plot.background=element_rect(fill = bcolor,color = NA),
  axis.title=element_blank(),
  axis.line=element_blank(),
  axis.text=element_blank(),
  axis.ticks=element_blank(),
  strip.text=element_markdown(size=8,color="black",hjust = 0.5),
  plot.margin=margin(t =5,r = 0,b = 5,l = 5)
)

ggplot(df %>% filter(year == 1957 | year == 1987 | year == 2017),
       mapping = aes(x = "",y = prop,fill = category)) +
  geom_col() +
  coord_polar("y") +
  scale_fill_npg()+
  guides(fill = "none") +
  facet_wrap(~ type)

ggsave("2021-08-10\\edspending.png",
       plot = last_plot(),
       device = "png",
       width = 5.5,
       height = 7,
       type = "cairo")
