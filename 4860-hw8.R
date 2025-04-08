#Name: Alex Cory
#Date: 4/7/2025
#Purpose: Stat 4860 HW8 Regression

library(Sleuth3)
library(ggplot2)

#Q1
#1.1. Identify the response variable (Force) as a character string.
q1.1 <- "Force"

#1.2. Identify the explanatory variable (Height) as a character string.
q1.2 <- "Height"

#1.3. Are these data are a random sample from some population?
q1.3 <- FALSE

# 1.4. Are these data are from a randomized experiment?
q1.4 <- FALSE

#Linear Model from base R
model1 <- lm(Force ~ Height, data = Sleuth3::ex0722)
summary_model1 <- summary(model1)

#1.5. Intercept
q1.5 <- coef(model1)[1]

#1.6. Slope
q1.6 <- coef(model1)[2]

#1.7.Estimated error variance.
residual_se <- summary_model1$sigma
q1.7 <- residual_se^2

#1.8. R-squared
q1.8 <- summary_model1$r.squared

first_obs <- Sleuth3::ex0722[1, ]

#1.9. Predicted value for the first observation.
q1.9 <- predict(model1, newdata = first_obs)

#1.10. Residual for the first observation.
q1.10 <- first_obs$Force - q1.9

#1.11. Cook's distance for the first observation.
q1.11 <- cooks.distance(model1)[1]

#1.12. Report the leverage value for the first observation.
q1.12 <- hatvalues(model1)[1]

#1.13. Create a ggplot scatterplot of Force vs Height.
q1.13 <- ggplot(data = Sleuth3::ex0722, aes(x = Height, y = Force)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Scatterplot of Force vs Height",
       x = "Height",
       y = "Force") +
  theme_minimal()

#Q2
#2.1. Scatterplot of Force vs Height
q2.1 <- ggplot(data = Sleuth3::ex0722, aes(x = Height, y = Force,
                                           color = Species, shape = Species)) +
  geom_point(size = 3) +
  labs(title = "Force vs. Height by Species",
       x = "Height",
       y = "Force") +
  theme_minimal()

#2.2. Is an interaction between Height and Species needed?
q2.2 <- FALSE

#2.3. Main effects regression model for Force on Height and Species.
model_main <- lm(Force ~ Height + Species, data = Sleuth3::ex0722)
q2.3 <- coef(model_main)

#2.4. Regression model with an interaction between Height and Species.
model_int <- lm(Force ~ Height * Species, data = Sleuth3::ex0722)
q2.4 <- coef(model_int)

#2.5. Report the p-value for the F-test for the interaction.
anova_comparison <- anova(model_main, model_int)
#The p-value is in the second row
q2.5 <- anova_comparison$"Pr(>F)"[2]

#2.6. 95% confidence interval for mean Force when Height = 8 for Species Cancer productus.
newdata_cancer <- data.frame(Height = 8, Species = "Cancer productus")
ci_cancer <- predict(model_int, newdata = newdata_cancer, interval = "confidence", level = 0.95)
q2.6 <- c(ci_cancer[1,'lwr'], ci_cancer[1,'upr'])

#2.7. 95% confidence interval for mean Force when Height = 8 for Species Lophopanopeus bellus.
newdata_bellus <- data.frame(Height = 8, Species = "Lophopanopeus bellus")
ci_bellus <- predict(model_int, newdata = newdata_bellus, interval = "confidence", level = 0.95)
q2.7 <- c(ci_bellus[1,'lwr'], ci_bellus[1,'upr'])

#2.8. 95% prediction interval for Force for a new observation when Height = 8
#for Species Lophopanopeus bellus.
ci_bellus <- predict(model_int, newdata = newdata_bellus, interval = "prediction", level = 0.95)
q2.8 <- c(ci_bellus[1,'lwr'], ci_bellus[1,'upr'])

#Q3
data <- read.csv("data.csv", header = TRUE)

#3.1. Observations
q3.1 <- nrow(data)

#3.2. Sample Mean
q3.2 <- mean(data$y)

#3.3. Standard Deviation
q3.3 <- sd(data$x)

#3.4. Regression
model_data <- lm(y ~ x, data = data)
q3.4 <- coef(model_data)
