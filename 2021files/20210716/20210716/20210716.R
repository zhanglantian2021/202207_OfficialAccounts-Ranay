library(tidyverse)
library(Ternary)

scooby <- read_csv('scoobydoo.csv')

df <- scooby %>%
  select(index, starts_with("unmask_"), ends_with("_amount")) %>%
  select(-unmask_other) %>%
  pivot_longer(cols = starts_with("unmask_"),
    names_to = "who_unmasked",
    values_to = "tmp",
    names_prefix = "unmask_") %>%
  filter(tmp != "FALSE") %>%
  select(-tmp) %>%
  group_by(who_unmasked) %>%
  summarise(
    monsters = sum(monster_amount),
    suspects = sum(suspects_amount),
    culprits = sum(culprit_amount)
  ) %>% janitor::adorn_percentages(denominator = "col") %>%
  select(monsters, suspects, culprits, who_unmasked) %>%
  add_column(color = cbPalette8[c(1, 7, 4, 6, 3)]) %>%
  mutate(who_unmasked = str_to_title(who_unmasked))

triangle <- matrix(c(50,50,0,50,0,
                     50,0,50,50),ncol=3,byrow = TRUE)


TernaryPlot(
  alab = "A \u2192",
  blab = "B \u2192",
  clab = "\u2190 C",
  lab.cex = 1.5,
  col = "#FFFFFF",
  grid.lines = 10,
  grid.col = "#DDDDDD",
  grid.lty = "solid",
  grid.lwd = 1,
  grid.minor.lines = 4,
  grid.minor.col = "#EEEEEE",
  grid.minor.lty = "solid",
  grid.minor.lwd = 1)

TernaryPolygon(triangle, border = 'green')
TernaryLines(list(c(0, 50, 0), c(50, 0, 50)), lty = 2, col = "red")
TernaryLines(list(c(0, 0, 50), c(50, 50, 0)), lty = 2, col = "blue")
TernaryLines(list(c(50, 0, 0), c(0, 50, 50)), lty = 2, col = "yellow")

TernaryPoints(df[1:3], col = df$color, pch = 16, cex = 1.5)
TernaryText(df[,1:3],
            labels = df$who_unmasked,
            col = df$color, pos = 4,cex =1)

