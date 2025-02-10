# Stats 4860 Homework 2: Data Visualization
## 2/10/2025

library(ggplot2)
library(dplyr)



q1 <- ggplot(iris, aes(x = Petal.Length, y = Petal.Width, shape = Species, color = Sepal.Length)) +
  geom_point(size = 3) +
  labs(
    title = "Iris Data Collected by Edgar Anderson (1936)",
    x = "Petal Length (cm)",
    y = "Petal Width (cm)",
    color = "Sepal Length (cm)",
    shape = "Species"
  ) +
  scale_x_continuous(limits = c(0, 8)) +
  scale_y_continuous(limits = c(0, 3))

q1

ggsave(plot = q1,
       filename = "petal-length-by-petal-width.jpeg",
       width = 6,
       height = 4)



Indometh <- Indometh %>%
  mutate(Subject = paste("Subject:", Subject))

q2 <- ggplot(Indometh, aes(x = time, y = conc)) +
  geom_point() +
  facet_wrap(~ Subject) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "Indomethacin Concentration Over Time by Subject",
    x = "Time (hours)",
    y = "Concentration (mcg/mL)"
  ) +
  theme_minimal()

q2

ggsave(plot = q2,
       filename = "Indomethacin-Concentration-Over-Time.jpeg",
       width = 6,
       height = 4)

q3 <- ggplot(diamonds, aes(x = price)) +
  geom_histogram(bins = 100) +
  facet_grid(cut ~ clarity, scales = "free_y") +
  scale_x_log10() +
  labs(
    title = "Distribution of Diamond Prices by Cut and Clarity",
    x = "Price (USD)",
    y = "Count"
  ) +
  theme_minimal()

q3

ggsave(plot = q3,
       filename = "diamond-cut-price.jpeg",
       width = 6,
       height = 4)
