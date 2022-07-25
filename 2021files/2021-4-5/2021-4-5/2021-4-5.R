library(tidyverse)

deaths <- read.delim("deaths.xls",header = T,sep="\t")

dat <- deaths %>%
  filter(state == "United States" & year == "2020") %>% 
  select(state,year, month, all_cause,
         heart_disease,starts_with("covid")) %>% 
  mutate(covid = covid_other + covid_only,
         all_other = all_cause - covid - heart_disease) %>% 
  select(-c(all_cause, covid_other,covid_only)) %>% 
  pivot_longer(cols = c("heart_disease","covid", "all_other"),
               names_to = "cause",
               values_to = "number") %>% 
  group_by(month) %>% 
  mutate(label_y = sum(number) + 40000,
         month = factor(month.abb[month], levels = month.abb))

ggplot(dat, aes(x = month, y = number, fill = cause)) +
  geom_col(color = "#5B5A5A",
           width = 1) +
  geom_text(aes(label = month, y = label_y), 
            family = "Deckhouse Regular") +
  scale_fill_manual(name = NULL,
                    values = c("#87c0e6","#ffa0aa","#808080"),
                    labels = c("All other deaths","COVID-19","Heart disease")) +
  theme_void() + 
  theme(
    legend.position = c(.5,.1),
    legend.direction = "horizontal"
  ) +
  coord_polar()
