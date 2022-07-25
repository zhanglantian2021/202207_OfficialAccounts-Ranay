library(ggplot2)
library(grid)
library(ggthemes)

reds <- c("#7B0664", "#E32219")
g <- rasterGrob(reds, width = unit(1, "npc"), height = unit(1, "npc"), interpolate = TRUE)
p <- ggplot(data = economics, aes(x = date, y = unemploy)) +
  geom_line( alpha=1, color = "black", size = 0.5 ) +
  xlab("Years") + ylab("Unemployed [thousands]") +
  theme_base() + 
  theme(panel.background=element_blank(),
        panel.border = element_blank(),
        plot.background=element_blank(),
        text = element_text(colour="white"),
        line = element_line(colour="white")) +
  theme()
p

grid.newpage()
grid.draw(g)
print(p, newpage = FALSE)


library(grid) 
g <- rasterGrob(blues9, width=unit(1,"npc"), height = unit(1,"npc"), 
                interpolate = TRUE) 
# grid.draw(g) 

library(ggplot2) 
ggplot(mtcars, aes(factor(cyl))) + # add gradient background 
  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
  geom_bar()



