library(tidyverse)
library(glue)
library(ggtext)
library(GGally)

data <- read_tsv("data.xls") %>%
  mutate(bump_A = case_when(percent_A < percent_B ~ percent_A - 2,
                                 percent_A > percent_B ~ percent_A + 2,
                                 TRUE ~ NA_real_),
         bump_B= case_when(percent_A < percent_B ~percent_B + 2,
                                  percent_A > percent_B ~percent_B - 2,
                                  TRUE ~ percent_B + 2),
         y_position = rev(1:nrow(.)))

arrows_data <- data %>%
  filter(abs(percent_A - percent_B) > 1) %>%
  mutate(midpoint = (percent_A + percent_B)/2) %>%
  select(country, y_position, percent_A, midpoint) %>%
  pivot_longer(c(percent_A, midpoint), names_to="type", values_to="x")

data %>% pivot_longer(cols = -c(country, y_position),
                      names_to=c(".value", "type"),names_sep = "_") %>%
  drop_na() %>%
  ggplot(aes(x=percent, y=y_position,group=y_position)) +
  geom_stripped_rows(odd="grey90",even="white")+
  geom_line(color="grey", size=1.5, show.legend = FALSE) +
  geom_path(data=arrows_data, aes(x=x, y=y_position, group=y_position),
            color="#00A08A",arrow = arrow(angle = 30, length=unit(0.1,"in")),
            show.legend = FALSE) +
  geom_point(aes(color=type),size=3, show.legend = FALSE)+
  geom_text(aes(label=glue("{percent}%"), x=bump),size=3,show.legend = FALSE) +

  scale_color_manual(values=c("#FF0000", "#00A08A")) +
  scale_x_continuous(limits=c(50,100),breaks=seq(50,100,by=5),
                     labels=glue("{seq(50, 100, 5)}%"),expand = c(0, 0)) +
  scale_y_continuous(breaks = c(data$y_position, 0.5, data$y_position+0.5),
                     labels = c(data$country, rep("", length(data$y_position)+1)),
                     expand = c(0, 0),
                     limits=c(0.5, 13.5)) +
  labs(x=NULL, y=NULL)+
  theme_classic()+
  theme(panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin=unit(c(0.5,0.5,0.5,0.2),units=,"cm"),
        axis.ticks.y = element_line(color = c(rep(NA, nrow(data)),rep("black", nrow(data)+1))),
        axis.text = element_text(color="black"))
