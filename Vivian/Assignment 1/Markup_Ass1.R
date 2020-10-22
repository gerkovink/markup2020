#markup2020 - exercise 1 - monte carlo simulation
#vivian baars

library(tidyverse)
library(ggplot2)

set.seed(208)

samples <- matrix(NA, 1000, 100)
for (i in 1:100){
  samples[,i] <- rnorm(1000, 50, 15)
}

means <- colMeans(samples)
bias <- means - 50
n <- nrow(samples)
se <- 15 / sqrt(n)
ub <- means + 1.96 * se
lb <- means - 1.96 * se
results <- as_tibble(cbind(means, bias, lb, ub))
results <- results %>%
  mutate(mean_in_ci = lb < 50 & ub > 50)
colMeans(results)

table <- results[!results$mean_in_ci,]

ggplot(results, aes(y = means, x = 1:100, colour = mean_in_ci)) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = lb, ymax = ub)) +
  theme_minimal() +
  scale_colour_discrete(name = "Mean in CI", labels = c("No", "Yes")) +
  labs(x = "Simulation", y = "Mean", title = "Simulation 95% confidence interval") +
  geom_hline(yintercept = 50, size = 0.5, linetype = "dashed", colour = "darkgrey")


