library(tidyverse)
library(scales)
library(glue)
# install.packages("ggbump")
library(ggbump)
library(showtext)

woman_clr <- "#38D1B8"
man_clr <- "#DBBE61"
update_geom_defaults("text", list(family = "roboto condensed"))

salary_industry <- read.delim("salary_industry.txt")
salary_gender <- read.delim("salary_gender.txt")
summary_industry <- read.delim("summary_industry.txt")
summary_education <- read.delim("summary_education.txt")


final <- ggplot(data = salary_industry)+
  geom_segment(aes(x = -0.5, xend = -2, y = index_ind, yend = index_ind),
               color = "grey50", linetype = "13")+
  geom_label(aes(x = -0.5, y = index_ind, label = industry),
             hjust = 1, color = "grey90", label.size = 0,
             fill = "grey20", size = 4.5, family = "roboto condensed")+
  geom_text(aes(x = 2.8, y = index_edu+10, label = education),
            hjust = 0, color = "grey90", size = 4.5)+
  geom_text(aes(x = 1.15, y = index+5, label = salary),
            color = "grey90", family = "oswald", size = 4.5)+ 
  geom_sigmoid(aes(x = -0.4, xend = 0.8, y = index_ind,
                   yend = index+5, group = factor(group)),color = "grey40")+
  geom_sigmoid(aes(x = 1.5, xend = 2.7, y = index+5, yend = index_edu+10,
                   group = factor(group2)), color = "grey40")+
  geom_sigmoid(data = summary_industry,
               aes(x = -0.4, xend = 0.8,y = index_ind,
                   yend = index+5, group = factor(group),
                   color = avg_salary), size = 1,inherit.aes = FALSE)+
  geom_point(data = summary_industry,
             aes(x = -0.4,y = index_ind, color = avg_salary),
             size = 2, inherit.aes = FALSE)+
  geom_point(data = summary_industry,
             aes( x = 0.8, y = index+5,color = avg_salary),
             size = 2, inherit.aes = FALSE)+
  geom_sigmoid(data = summary_education,
               aes(x = 1.5,xend = 2.7,y = index+5,yend = index_edu+10,
                   group = factor(group),color = avg_salary),size = 1,inherit.aes = FALSE)+
  geom_point(data = summary_education,
             aes(x=1.5,y=index+5,color=avg_salary),size=2,inherit.aes=FALSE)+
  geom_point(data = summary_education,
             aes(x = 2.7,y = index_edu+10,color=avg_salary),size=2,inherit.aes=FALSE)+
  geom_segment(data = filter(salary_gender, gap_direction == "Woman"),
               aes(x = -2, xend = -2-gender_scale,y = index_ind-0.1,
                   yend = index_ind-0.1), color = woman_clr, size = 4)+
  geom_segment(data = filter(salary_gender, gap_direction == "Man"),
               aes(x = -2, xend = -2+gender_scale, y = index_ind-0.1,
                   yend = index_ind-0.1), color = man_clr, size = 4)+
  geom_text(data = filter(salary_gender, gap_direction == "Woman"),
            aes(x = -2, y = index_ind,label = dollar(gender_gap, accuracy = 1)),
            nudge_y = 0.3, color = woman_clr, hjust = 1.1) +
  geom_text(data = filter(salary_gender, gap_direction == "Man"),
            aes(x = -2, y = index_ind, label = dollar(gender_gap, accuracy = 1)),
            nudge_y = 0.3, color = man_clr, hjust = -0.1) +
  annotate("text", x = -2.2, y = 26, label = "Women", color = woman_clr, size = 5)+
  annotate("text", x = -1.8, y = 26, label = "Men", color = man_clr, size = 5) +
  annotate("text", x = -2, y = 27, label = "Average gender gap",color="grey80",
           size = 5, fontface = "bold") +
  scale_x_continuous(limits = c(-2.2,3.5))+
  scale_color_viridis_c(label = dollar_format()) +
  guides(color = guide_colorbar(title.position = "top",
                                title.hjust = 0.5,
                                barwidth = 15)) +
  labs(title = "Salary overview in the US by industry, education and gender gap",
       color = "Average annual salary")+
  theme_void()+
  theme(plot.background = element_rect(fill = "grey20", color = NA),
        plot.title = element_text(family = "oswald",size = 20,color = "grey80",
                                  hjust = 0.5,face = "bold",margin = margin(10,0,0,0)),
        legend.position = c(0.8, 0.13),
        legend.direction = "horizontal",
        legend.title = element_text(size = 12, color = "grey80"),
        legend.text = element_text(size = 10, color = "grey80"))

ggsave(final,file="final.pdf",dpi= 320,width = 17, height = 10, units = "in")
ggsave(final,file="final.png",dpi= 320,width = 17, height = 10, units = "in")


