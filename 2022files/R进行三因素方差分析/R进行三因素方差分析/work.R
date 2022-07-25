library(tidyverse)
library(ggthemes)
library(multcompView)
library(egg)


df <- read_tsv("co2.xls")

anova <- aov(uptake ~ factor(conc)*Type*Treatment, data = df)
summary(anova)

Tukey <- TukeyHSD(anova)
cld <- multcompLetters4(anova, Tukey)

dt <- group_by(CO2, conc, Type, Treatment) %>%
  summarise(uptake_mean=mean(uptake), sd=sd(uptake)) %>%
  arrange(desc(uptake_mean))
cld <- as.data.frame.list(cld$`factor(conc):Type:Treatment`)
dt$Tukey <- cld$Letters

ggplot(dt,aes(x = factor(conc), y = uptake_mean, fill = Type:Treatment)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymax = uptake_mean + sd, ymin = uptake_mean - sd),
                position = position_dodge(0.9), width = 0.25, color = "Gray25") +
  xlab(expression(CO[2]~Concentration~'('~mL~L^-1~')')) +
  ylab(expression(CO[2]~Uptake~'('~µmol~m^2~s^-1~')')) +
  scale_fill_brewer(palette = "Greens") +
  theme_few()

ggplot(dt, aes(x = factor(conc), y = uptake_mean, fill = Treatment)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymax = uptake_mean + sd, ymin = uptake_mean - sd),
                position = position_dodge(0.9), width = 0.25, color = "Gray25") +
  xlab(expression(CO[2]~Concentration~'('~mL~L^-1~')')) +
  ylab(expression(CO[2]~Uptake~'('~µmol~m^2~s^-1~')')) +
  scale_fill_brewer(palette = "Greens") +
  theme_few() +
  facet_grid(.~Type, labeller = label_both)

p <- ggplot(dt, aes(x = factor(conc), y = uptake_mean, fill = Treatment)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymax = uptake_mean + sd, ymin = uptake_mean - sd),
                position = position_dodge(0.9), width = 0.25, color = "Gray25") +
  xlab(expression(CO[2]~Concentration~'('~mL~L^-1~')')) +
  ylab(expression(CO[2]~Uptake~'('~µmol~m^2~s^-1~')')) +
  theme_few() +
  theme(legend.position = c(0.95,0.98),legend.justification = c(1, 1),
        legend.title = element_blank(),
        axis.text=element_text(color="black"),
        axis.title = element_text(color="black")) +
  scale_fill_manual(values = c("#C1D5A5", "#84A17C")) +
  scale_y_continuous(expand = expansion(0),limits = c(0,50),breaks = seq(0,50,5))+
  facet_grid(.~Type, labeller = label_both) +
  geom_text(aes(label=Tukey, y = uptake_mean + sd + 2), size = 3, color = "Gray25",
            show.legend = FALSE,position = position_dodge(0.9))

tag_facet(p, fontface = 1, tag_pool = c("(a) Quebec",
                                        "(b) Mississipi"),
          open = NULL, close = NULL, hjust = -0.05)

