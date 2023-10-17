# Packages
library(tidyverse)
library(mgcv)
library(performance)
library(DHARMa)
library(broom)


data("kyphosis", package = "rpart")
kyphosis <- transform(kyphosis, Kyphosis = as.numeric(factor(Kyphosis)) - 1)

ggplot(kyphosis, aes(x = Age, y = Number, color = Kyphosis)) +
  geom_point() +
  labs(title = "Kyphosis vs. Age and Number of Vertebrae")




fit_gam <- gam(Kyphosis ~ s(Age, k=4) + s(Number, k=4) + s(Start, k=4), 
               family = binomial, data = kyphosis)

par(mfrow = c(2, 2))
plot(fit)

check_model(fit)

residuals <- simulateResiduals(fittedModel = fit)
plot(residuals)

tidy_fit <- tidy(fit)
ggplot(tidy_fit, aes(x = term, y = estimate, ymin = estimate - std.error, 
                     ymax = estimate + std.error)) +
  geom_pointrange() +
  coord_flip() +
  labs(title = "Coefficient Plot")