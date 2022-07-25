library(tidyverse)
library(pBrackets)
library(glue)
library(ggtext)

bracketsGrob <- function(...){
  l <- list(...)
  e <- new.env()
  e$l <- l
  grid:::recordGrob(  {
    do.call(grid.brackets, l)
  }, e)
}

p <- iris %>% group_by(Species) %>%
  summarise(mean_Sepal.Length=mean(Sepal.Length),
            sd_Sepal.Length=sd(Sepal.Length)) %>% 
  ggplot(aes(Species, mean_Sepal.Length)) +
    geom_col(aes(fill=Species),width=0.5) +
    scale_y_continuous(limits=c(0,9),expand=c(0,0)) +
    theme_minimal() +
    labs(x = "Species",y = "mean_Sepal.Length",
      title = "The infamous Iris plot", caption = "2021-7-4") +
    theme(axis.title.x = element_markdown(color="red",vjust=0.5),
      axis.line = element_line(color = "#3D4852"),
      axis.ticks = element_line(color = "#3D4852"),
      panel.grid.major.y = element_line(color = "#DAE1E7"),
      panel.grid.major.x = element_blank(),
      plot.title = element_text(size = 20, face = "bold",
        margin = margin(b = 30)),
      plot.margin = unit(rep(1, 4),"cm"),
      axis.text = element_text(size = 13, color = "#22292F"),
      axis.title = element_text(size = 12, hjust = 1),
      axis.title.y = element_text(margin = margin(r = 12)),
      axis.text.y = element_text(margin = margin(r = 5)),
      axis.text.x = element_text(margin = margin(t = 5)),
      plot.caption = element_text(
        size = 12, face = "italic",
        color = "#606F7B", margin = margin(t = 12)),
      legend.position="top")+
    scale_x_discrete(limits=c("setosa","virginica","versicolor"))+
    scale_fill_brewer(palette="Blues")


p

b1 <- bracketsGrob(x1=0.1,y1=0.1,x2=0.1,y2=0.72,h=0.025, 
                   lwd=2,col="black")

b2 <- bracketsGrob(x1=0.2,y1=0.95,x2=0.5,y2=0.95,
                   h=0.05,lwd=2, col="black")

b3 <- bracketsGrob(x1=0.5,y1=0.08,x2=0.81,y2=0.08,
                   h=-0.05,lwd=2, col="black")

b4 <- bracketsGrob(x1=0.9,y1=0.1,x2=0.9,y2=0.95,
                   h=-0.05,lwd=2, col="black")

p + annotation_custom(b1)+
  annotation_custom(b2)+
  annotation_custom(b3)+ 
  annotation_custom(b4)+
  scale_y_continuous(expand=c(0.1,0))
