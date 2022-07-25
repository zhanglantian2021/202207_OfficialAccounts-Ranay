library(tidyverse)
library(grid)
library(ggtext)

chocolate <- read_csv('chocolate.csv')

mean <- mean(chocolate$rating)


df <- chocolate  %>%
  group_by(company_location) %>% 
  summarise(n = n(),min_rating = min(rating),max_rating = max(rating),
            avg_rating = mean(rating, na.rm = T)) %>% 
  mutate(company_location = fct_reorder(company_location, avg_rating)) %>% 
  filter(n > 3) %>% 
  mutate(rating_diff = avg_rating - mean) %>% 
  filter(abs(rating_diff) >0.05)


df %>% 
    ggplot() + 
    geom_col(aes(x = rating_diff, y = company_location, fill = rating_diff > 0),
             size = 0.25, color = "white")+
  geom_point(aes(x = rating_diff,y = company_location,
                 color=ifelse(rating_diff > 0,"#E11B4D","#8456BA")),size=5)+
    geom_text(aes(x = ifelse(rating_diff > 0, -.005, .005),y = company_location, 
                  label = company_location,
                  color=ifelse(rating_diff > 0,"#E11B4D","#8456BA"),
                  hjust = ifelse(rating_diff > 0, 1, 0)),size = 4)+
    geom_vline(xintercept=0,size=1,color="grey40")+
    scale_x_continuous(expand = expansion(add = c(0,.2)),
                       breaks = seq(-.4,.2, by = .2)) + 
    scale_y_discrete(expand = c(.025,.025))+
    scale_fill_manual(values = c("TRUE" = "#E11B4D","FALSE" = "#8456BA"))+
    scale_color_manual(values = c("#8456BA","#E11B4D"))+
    coord_cartesian(clip = "off") +  
    theme_minimal() + 
    theme(panel.grid = element_blank(),
          plot.background = element_rect(fill="Aliceblue",color="Aliceblue"),
      axis.text.y =  element_blank(),
      axis.title = element_blank(),
      legend.position = "none",
      axis.text.x = element_text(face = "bold", size =rel(1), color = "black"))


