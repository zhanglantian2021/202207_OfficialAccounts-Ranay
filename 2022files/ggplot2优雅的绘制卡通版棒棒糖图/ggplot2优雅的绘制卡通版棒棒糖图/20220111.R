package.list=c("tidyverse","corrplot","corrr","RColorBrewer","showtext","magrittr")

for (package in package.list) {
  if (!require(package,character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

df <- read_csv("data.csv") %>% select(danceability:tempo) %>% select(-mode,-key)

df %<>% correlate() %>% 
  pivot_longer(-term, names_to = "group", values_to = "corr") %>%
  filter(term == "danceability" & !is.na(corr)) %>%
  mutate(group = str_to_title(group),
         budge = case_when(corr > 0 ~ -.075,corr < 0 ~ .075, TRUE ~ 0),
         align = case_when(corr > 0 ~ 1,corr < 0 ~ 0, TRUE ~ 0))

df %>% ggplot(aes(reorder(group, corr), corr)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_segment(aes(x = reorder(group, corr), xend = reorder(group, corr),
                   y = 0, yend = corr),linetype = 4, size = 1, color = "grey50") +
  geom_point(size = 12, aes(fill = corr), shape = 21) +
  geom_text(aes(label = round(corr, digits = 2),
                color = ifelse(abs(corr) < .2, "black", "white")),
            size = 2, family = "rs") +
  geom_text(aes(label = group, 
                y = budge, hjust = align), family = "rs", color = "black") +
  scale_fill_gradient2(low = "#D12424", mid = "white", high = "#f28bd5") +
  scale_color_manual(values = c("black", "black")) +
  scale_y_continuous(breaks = c(-.45, .55), labels = c("Less Danceable", "More Danceable")) +
  theme_minimal() + 
  theme(legend.position = "none",
        text = element_text(family = "rs", color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.line.x = element_line(arrow = arrow(length = unit(.3, "cm"), ends = "both"),
                                   color = "black")) +
  coord_flip()
