library(tidyverse)
library(countrycode)
library(circlize)
library(ggplot2)
library(cowplot)
library(grid)
library(ggplotify)


df <- readr::read_csv('erasmus.csv')

top_countries <- c("Germany","Italy","Spain","Poland","United Kingdom","Romania","Belgium","France","Netherlands (the)","Austria" ,"Luxembourg","Lithuania","Czechia")

data<-df%>%
  mutate(
    to= countrycode(receiving_country_code, origin="iso2c", destination="iso.name.en"),
    from= countrycode(sending_country_code , origin="iso2c", destination="iso.name.en"))%>%
  mutate(
    to = replace(to, receiving_country_code=="UK","United Kingdom"),
    from = replace(from, sending_country_code=="UK","United Kingdom"), 
    to = replace(to, receiving_country_code=="EL","Greece"),
    from = replace(from, sending_country_code=="EL","Greece")
  )%>%
  group_by(from, to)%>%
  summarise(value=sum(participants))%>%
  arrange(-value)

chord_data<-data%>%
  filter(from!=to)%>%
  filter(from %in% top_countries & to %in% top_countries)%>%
  arrange(-value)

chord_data[chord_data=="Netherlands (the)"]<-"Netherlands"

pal<-c("#002765","#0061fd","#1cc6ff","#00b661","#5bf34d","#ffdd00","#ff7d00","#da2818","#ff006d","#8f00ff","#453435","black","grey80")

chordDiagram(chord_data, grid.col = pal)

p<- recordPlot()

as.ggplot(ggdraw(p))+
  theme(text=element_text(family="Arial"),
        plot.margin   =margin(t=20))

