library(multcompView)
library(stats)
library(tidyverse)
library(ggsci)

data2 <- ToothGrowth
data2$dose = as.factor(data2$dose)

anova <- aov(len ~ supp*dose, data = data2) #two way anova
summary(anova) #represents ANOVA

tukey <- TukeyHSD(anova)

group_lettering <- multcompLetters4(anova, tukey) #
group_lettering
group_lettering2 <- data.frame(group_lettering$`supp:dose`$Letters)
group_lettering2

mean_data2 <- data2 %>% 
  group_by(supp, dose) %>% 
  summarise(len_mean=mean(len), sd = sd(len)) %>%
  arrange(desc(len_mean))

mean_data2$group_lettering <- group_lettering2$group_lettering..supp.dose..Letters


ggplot(mean_data2,aes(x = dose, y = len_mean,group=supp))+
  geom_bar(position=position_dodge(0.9),stat = "identity", aes(fill = supp), 
           show.legend = TRUE)

ggplot(mean_data2, aes(x = dose, y = len_mean,group=supp))+
  geom_bar(position=position_dodge(0.9),stat = "identity",
           aes(fill = supp),show.legend = TRUE) +
  geom_errorbar(aes(ymin = len_mean-sd, ymax=len_mean+sd),width = 0.1,
                position=position_dodge(0.9))

ggplot(mean_data2, aes(x = dose, y = len_mean,group=supp))+
  geom_bar(position=position_dodge(0.9),stat = "identity",
           aes(fill = supp), show.legend = TRUE) +
  geom_errorbar(aes(ymin = len_mean-sd, ymax=len_mean+sd),
                width = 0.1, position=position_dodge(0.9))+
  geom_text(aes(label = group_lettering, y = len_mean + sd),
            vjust=-0.4,position=position_dodge(0.9))


ggplot(mean_data2, aes(x = dose, y = len_mean,group=supp))+
  geom_bar(position=position_dodge(0.9),stat = "identity",
           aes(fill = supp), show.legend = TRUE) +
  geom_errorbar(aes(ymin = len_mean-sd, ymax=len_mean+sd),
                width = 0.1, position=position_dodge(0.9)) + 
  geom_text(aes(label = group_lettering, y = len_mean + sd),
            vjust=-0.4, position=position_dodge(0.9)) +
  scale_y_continuous(expand = expansion(0),limits = c(0,35),
                     breaks = seq(0,35,5))+
  labs(x=NULL,y=NULL)+
  theme_minimal()+
  theme(
    plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"),
    panel.background = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 10, color = "black",face = "bold"),
    axis.text = element_text(size = 10,color = "black"),
    axis.text.x = element_text(margin=margin(t =3)),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.ticks.x = element_blank())+
  scale_fill_jco()

  


