# Zoe Dunias 

# Exercise 1.8 Monte Carlo simulation exercise

# Load packages
library(ggplot2)
library(tidyverse)
library(knitr)
library(kableExtra)

# 1.a.
mu <- 0  
SD <- 1
N <- 10000
n_sim <- 100

set.seed(100) # Set seed for reproducibility

sm <- replicate(n_sim, rnorm(N, mu, SD)) # Sample 100 samples from a standard normal distribution

# 1.b.
# For each of the samples, calculate the difference between sample mean and population mean (0)
M <- colMeans(sm) 
bias <- abs(M - mu)

SE <- SD/sqrt(N) # Calculate standard error

CI_low <- M + qnorm(0.025) * SE # Calculate lower bound of the 95% confidence interval
CI_high <- M + qnorm(0.975) * SE # Calculate higher bound of the 95% confidence interval


# 1.c
# Select samples where the sample mean contains 0 (population mean)
contains0 <- CI_low < mu & mu < CI_high

stat_results <- data.frame(M, bias, SE, CI_low, CI_high, contains0) # Create a dataframe containing the calculated results

boundaries <- aes(ymax = CI_high, ymin = CI_low) # Indicate the range of the confidence intervals in the y-axis

# Create plot that demonstrates:
# "A replication of the procedure that generates a 95% confidence interval that is centered around the sample mean would cover the population value at least 95 out of 100 times" (Neyman, 1934)
# For 5 of the 100 samples, the confidence interval does not contain 0 (the population mean).
ggplot(data = stat_results, 
       aes(y=M, x=1:n_sim, colour = contains0)) + 
  geom_hline(aes(yintercept = mu), 
             color = "black", 
             size = 1) + 
  geom_pointrange(boundaries) + 
  xlab("Simulated samples") +
  ylab("Means and corresponding 95% confidence intervals")

# 1.d
# Extract the sample statistics of samples where confidence interval does not contain 0 
not_0 <- stat_results %>% filter(contains0 == F)

# Create table
not_0 %>%  kable() %>%
    kable_styling(position = "float_left", full_width = FALSE)

