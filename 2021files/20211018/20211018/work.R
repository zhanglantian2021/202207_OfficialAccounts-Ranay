library(tidyverse)
library(ggthemes)
library(hrbrthemes)
library(ggrepel)
library(ggtext)

nurses <- readr::read_csv('nurses.txt')

nurses1 <- nurses %>% 
  janitor::clean_names() %>% 
  filter(!state %in% c("Puerto Rico", "Guam", "Virgin Islands")) %>% 
  filter(!is.na(hourly_wage_median)) %>% 
  group_by(year) %>% 
  mutate(h_wage_max = if_else(hourly_wage_median == max(hourly_wage_median), TRUE, FALSE),
         h_wage_min = if_else(hourly_wage_median == min(hourly_wage_median), TRUE, FALSE)) %>% 
  filter(h_wage_max == TRUE | h_wage_min == TRUE) %>% 
  dplyr::select(state, year, total_employed_rn, hourly_wage_median) 

nurses2 <- nurses %>% 
  janitor::clean_names() %>% 
  filter(!state %in% c("Puerto Rico", "Guam", "Virgin Islands")) %>% 
  filter(!is.na(hourly_wage_median)) %>% 
  group_by(year) %>% 
  mutate(h_wage_max = if_else(hourly_wage_median == max(hourly_wage_median), TRUE, FALSE),
         h_wage_min = if_else(hourly_wage_median == min(hourly_wage_median), TRUE, FALSE)) %>% 
  filter(h_wage_max == FALSE & h_wage_min == FALSE) %>% 
  dplyr::select(state, year, total_employed_rn, hourly_wage_median) 


fig1 <- nurses2 %>% 
  ggplot(aes(x = year,  y = hourly_wage_median, size = total_employed_rn)) +
  geom_jitter(alpha = 0.02) +
  geom_line(data = nurses1,aes(x = year,y=hourly_wage_median,group=year),
            size=1,alpha=0.1) +
  geom_point(data = nurses1,aes(x=year,y=hourly_wage_median,color=state),
             show.legend=FALSE)+
  labs(y=NULL,x=NULL,size = NULL,title = NULL,subtitle = NULL,caption = NULL) +
  scale_y_continuous(limits=c(0,65),
                     labels=scales::number_format(prefix = "US$ ")) +
  scale_x_continuous(limits=c(1998,2023)) +
  scale_size_continuous(name = "Total employed registered nurses",
                        breaks = c(50000,150000,250000,350000),limits = c(0,350000),
                        labels = c("50K","150K","250K","350K"),
                        guide = guide_legend(title.position = "top",
                                             verride.aes = list(colour = "#333333",alpha = 0.5))) +
  theme_minimal() +
  scale_color_calc() +
  theme(legend.position = c(0.27,0.83),
        legend.direction = "horizontal",
        legend.title = element_markdown(size =10,face = "bold"),
        legend.text = element_markdown(size = 10),
        axis.text=element_text(size =10,color="black"))

scales::show_col(calc_pal()(7))

fig1 +
  annotate("text", x = 2010, y = 46,label="California",size=5,
           color = "#ff420e",fontface = "italic") +
  annotate("text", x = 2017, y = 53, label = "Hawaii", size =5,
           color = "#ffd320",fontface = "italic") +
  annotate("text",x=2000.4,y=32,label="Maryland",size=5,
           color="#7e0021",fontface = "italic") +
  annotate("text",x=2014, y = 22,label = "South Dakota",size = 5,
           color="#314004",fontface = "italic") +
  annotate("text",x = 2006, y = 19,label = "Iowa", size =5,
           color="#579d1c",fontface = "italic") +
  annotate("text", x = 2020, y = 25, label = "Mississippi",size =5,
           color = "#83caff",fontface = "italic") +
  annotate("text", x = 2022.2, y = 29, 
           label = "Alabama", size =5,color = "#004586",fontface = "italic") +
  annotate("text", x = 2013, y = 34,label = "Other states", 
           size=5,color = "black",fontface = "italic")
