library(tidyverse)
library(scales)
library(ggtext)
library(patchwork)
library(cowplot)
library(RColorBrewer)

mycolors <- colorRampPalette(brewer.pal(12,"Paired"))(21)

df <- tribble(~group,~value,
        "A", 40,
        "B",60)

p1 <- df %>% arrange(desc(value)) %>% 
  ggplot(.,aes(x="",y=value,fill=group))+
  geom_bar(width=1,stat="identity")+
  coord_polar("y",start=0)+
  geom_text(aes(y = value/2 + c(0, cumsum(value)[-length(value)]), 
                label = percent(value/100)), size=4)+
  scale_fill_manual(values = mycolors)+
  theme(axis.text.x=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        legend.position = "non",
        legend.title = element_blank(),
        legend.text = element_text(color="black",size=9), 
        legend.spacing.x=unit(0.1,'cm'),
        legend.key.width=unit(0.5,'cm'),
        legend.key.height=unit(0.5,'cm'), 
        legend.background=element_blank())

df2 <- read_tsv("otu_taxa_table.xls") %>% 
  select(OTU:C5,taxonomy) %>% 
  separate(taxonomy,
           into=c("domain","phylum","class","order","family","genus","species"),sep=";") %>% 
  mutate_at(vars(c(`domain`:`species`)),~str_split(.,"__",simplify=TRUE)[,2]) %>% 
  select(A1:C5,genus) %>% drop_na() %>% 
  group_by(genus) %>%  filter(genus !="") %>%  
  count() %>% ungroup() %>% mutate(value=n/sum(n)) %>% arrange(desc(value)) %>%
  mutate(group=case_when(value <  0.0088999644 ~ "others",
                         TRUE ~ as.character(genus))) %>% 
  group_by(group) %>% summarise(value=sum(value)) %>% arrange(desc(value))

df2$group <- factor(df2$group,levels = df2$group)

p2 <- df2 %>% ggplot(.,aes(x="",y=value,fill=group))+
  geom_bar(width=1,stat="identity")+
  coord_polar("y",start=0)+
  scale_fill_manual(values = mycolors)+
  theme(axis.text.x=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        legend.position = "non",
        legend.title = element_blank(),
        plot.margin=unit(c(0,13,0,0),units=,"cm"),
        legend.text = element_text(color="black",size=9), 
        legend.spacing.x=unit(0.1,'cm'),
        legend.key.width=unit(0.5,'cm'),
        legend.key.height=unit(0.5,'cm'), 
        legend.background=element_blank())


p2 %>% ggdraw()+draw_plot(p1,scale=0.4,x=0.38,y=0)
  