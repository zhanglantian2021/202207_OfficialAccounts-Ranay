package.list=c("tidyverse","wesanderson",
               "lubridate","ggstream","ggwaffle",
               "waffle","extrafont")

for (package in package.list) {
  if (!require(package,character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

scooby<-readr::read_csv("Scooby-Doo Completed.csv",
                        na = c("", "NA"))%>%
  mutate(year=as.numeric(substr(date.aired,1,4)))%>%
  mutate(Gender=if_else(str_detect(monster.gender,"Male,Female|Female,Male"),"Mixed",monster.gender))%>%
  mutate(Gender=gsub(".*,","",Gender))%>%
  mutate(monster.species=gsub(".*,","",monster.species))%>%
  select(year,monster.amount,monster.gender,
         monster.species,format,Gender)


scooby %>%
  ggplot(aes(fill=Gender,values=monster.amount))+
  waffle::geom_waffle(n_rows=30,size = 0.1,colour = "white",flip = T)+
  scale_fill_manual(name = NULL,
                    values = c(wes_palette("Darjeeling1",4)))+
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text =element_blank(),
        legend.text = element_text(colour = "#373737",
                                   size=10),
        axis.ticks = element_blank(),
        legend.key.width = unit(0.5,"cm"),
        legend.key.size = unit(0.5,"cm"))

iris$Species <- as.character(iris$Species)

waffle_data <- waffle_iron(iris,aes_d(group = Species))

ggplot(waffle_data,aes(x,y,color=group))+
  geom_tile(color="grey70",fill="white",size=0.01)+
  geom_point(aes(size=10),shape=15)+
  geom_point(size=6,shape=15)+
  labs(x=NULL,y=NULL)+ 
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0),position="right")+
  scale_color_manual(name = NULL,
                     values = c(wes_palette("Darjeeling1",4)))+
  scale_size(range=c(1,13.5),guide=NULL)+
  theme(text=element_text(family="Roboto"),
        panel.background=element_blank(),
        axis.text.x=element_text(color="black"),
        axis.text.y=element_text(color="black"),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        legend.title=element_blank(),
        legend.text=element_text(size=10),
        legend.key=element_blank(),
        legend.spacing.x=unit(0.05,'cm'),
        legend.key.width = unit(0.05,"cm"),
        legend.key.height = unit(0.5,"cm"))

