library(tidyverse)
library(ggtext)

df <- read_tsv("data.xls")

wet_df <- df %>% 
    filter(str_starts(code, 'W')) %>% 
    mutate(mean_probability = mean_probability * -1)


drought_df <- df %>% 
    filter(str_starts(code, 'D')) 


drought_df %>% 
    ggplot(aes(x = year, y = mean_probability, fill = legend_text)) +  
    geom_area(alpha = 0.95)+
    geom_line(position = "stack", size = 0.1, color = 'gray40') +
    geom_area(data = wet_df, alpha = 0.95) +
    geom_line(data = wet_df, position = "stack", size = 0.1, color = 'gray40')+
    scale_x_continuous(expand= c(0,0), breaks=seq(1895,2022,25), 
                       limits=c(1895,2022),position = 'top')+
    scale_y_continuous(expand = c(0.05, 0.05),breaks = seq(0, 100, 25))+
    scale_fill_manual(values = c("#BA7A70","#9D4E3F","#7D2D1E","#621B0B","#421401","#829BAB",
                                 "#568199","#35627F","#1D4866","#0A2D46")) +
    guides(fill = guide_legend(nrow = 2,byrow = TRUE))+ 
    labs(x=NULL,y =NULL) +
    theme_minimal()+
    theme(
        plot.background= element_rect(fill = "#ecebec", color = "#ecebec"),
        panel.background= element_rect(fill = "#ecebec", color = "#ecebec"),
        legend.position = 'bottom',
        legend.title= element_blank(),
        legend.text = element_text(size =9,color="black"),
        legend.background = element_rect(fill="#ecebec", color = "#ecebec"),
        panel.grid= element_blank(),
        plot.margin=margin(50,20,50,20),
        axis.line.x=element_line(color="gray40", size = 1),
        axis.ticks.x=element_line(color="gray40"),
        axis.text.y=element_blank(),
        axis.text.x=element_text(color="black")) 


