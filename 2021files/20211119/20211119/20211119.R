package.list=c("tidyverse","janitor","gggibbous","scales","showtext","ggtext")

for (package in package.list) {
  if (!require(package,character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

df <- read_csv("data.xls") %>% 
  clean_names() %>% 
  select(year, total_enrollment, males, females)


df %>% 
  mutate(prop_females = females / total_enrollment) %>% 
  ggplot(aes(year, total_enrollment)) +
  geom_line(color = "#003049", size = 1.05) +
  geom_moon(aes(ratio = prop_females), right = TRUE, size = 7, fill = "#FF0000", color = NA)+
  geom_moon(aes(ratio = 1 - prop_females), right = FALSE, size = 7, fill = "#00A08A", color = NA)+
  scale_y_continuous(name = "", limits = c(200000, 350000), labels = number_format(scale = 1/1000, suffix = "k")) +
  scale_x_continuous(name = "", breaks = seq(1980, 2015, 5)) +
  theme_minimal() +
  theme(
    plot.margin = margin(0.5,0.5,0.5,0.5),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#eae2b7", color = "#eae2b7"),
    axis.text=element_text(color="black"))

df
