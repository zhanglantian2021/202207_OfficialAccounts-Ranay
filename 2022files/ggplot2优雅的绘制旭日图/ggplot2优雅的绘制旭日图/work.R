library(tidyverse)
library(geomtextpath)
library(ggsci)

load("da.Rdata")

ggplot()+
  geom_rect(data=data.frame(xmin=0, xmax=1, ymin=0, ymax=.75), 
            mapping=aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="white")+
  geom_rect(data=houses, mapping=aes(xmin=start_lim, xmax=end_lim, ymin=1, ymax=1.75, 
                        fill=fill_col), color="white")+
  geom_rect(data=parts, mapping=aes(xmin=start_lim, xmax=end_lim, ymin=1.90, ymax=2.75, 
                        fill=fill_col), color="white")+
  geom_textpath(data=houses, mapping=aes(x=mid_pt, y=1.35,label=toupper(speaker_type)), color="black",
                size = 3, text_only = TRUE, family="Gill Sans") +
  geom_textpath(data=houses %>% filter(speaker_type!="Neutral"),mapping=aes(x=mid_pt, y=1.35,label=toupper(speaker_type)),
                upright=FALSE, color="white", size = 3, text_only = TRUE, family="Gill Sans") +
  geom_textpath(data=parts %>% filter(speaker_type!="Neutral" & perc>=0.02),
                mapping=aes(x=mid_pt, y=2.35,label=paste0(str_replace(speaker," ","\n"),"\n",round(perc*100,1),"%")),
                color="white",size =3, text_only = TRUE) +
  geom_textpath(data=parts %>% filter(speaker_type=="Neutral" & perc>=0.025),
                mapping=aes(x=mid_pt, y=2.35, label=paste0(str_replace(speaker," ","\n"),"\n",round(perc*100,1),"%")),
                color="black",size =3, text_only = TRUE) +
  scale_fill_jco()+
  scale_color_jco()+
  coord_polar()+
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_rect(fill="white"))
