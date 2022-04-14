library(tidyverse);
library(ggplot2);
library(plotly)

yemen_data <- read.csv("yemen_data.csv")
new_haiti_data <- read.csv("haiti_data.csv")

haiti_data <- new_haiti_data %>% 
  mutate(death_rate = Deaths/Cases)

yemen_t.test <- t.test(yemen_data$attack_rate_fraction, yemen_data$death_rate, alternative = "two.sided", var.equal = TRUE)

yemen_cor.test <- cor.test(yemen_data$attack_rate_fraction, yemen_data$death_rate, method="pearson")

yemen_spearman <- spearman <- cor.test(yemen_data$attack_rate_fraction, yemen_data$death_rate, method = "spearman")

yemen_plot <- ggplot(data = yemen_data, mapping=aes(x=attack_rate_fraction, y=death_rate)) +
  geom_point(color="sky blue") +
  geom_smooth(method=lm, formula = y ~ x, color='black') +
  labs(x="Attack Rate", y="Death Rate") +
  ggtitle("Attack Rate vs Death Rate")

#Attack Rate vs HDI
haiti_t.test <- t.test(haiti_data$HDI.1, haiti_data$death_rate, alternative = "two.sided", var.equal = TRUE)

haiti_cor.test <- cor.test(haiti_data$HDI.1, haiti_data$death_rate, method = "pearson")

haiti_spearman <- spearman <- cor.test(haiti_data$HDI.1, yemen_data$death_rate, method = "spearman")

haiti_plot <- ggplot(data = haiti_data, mapping=aes(x=HDI.1, y=Death.1000)) +
  geom_point(color="#4DB6D0", size=5) +
  geom_smooth(method=lm, formula = y ~ x, color='black') +
  labs(x="Human Development Index (HDI)", y="Deaths per 1000") +
  ggtitle("Correlation Between Deaths per 1000 in Haiti and HDI") +
  theme_classic() +
  theme(legend.position = "none")

haiti_plot
