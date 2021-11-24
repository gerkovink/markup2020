### Rianne Kraakman
### Markup Languages and Reproducible Programming in Statistics
### Assignment 1 Exercise 8 - Monte Carlo simulation


#Set seed
set.seed(678)

#Sample 100 samples (n=1000) from a standard normal distribution
samples <- replicate(100, rnorm(1000, mean = 0, sd = 1))

#For each of the samples, calculate the following statistics for the mean
#absolute bias
#standard error
#lower bound 95CI
#upper bound 95CI

#Function to get this information
sampleinfo <- function(x){
  M <- mean(x)
  BIAS <- abs(M - 0)
  SE <- 1/sqrt(1000)
  lb95 <- M - 1.96*SE
  ub95 <- M + 1.96*SE
  return(c(M, BIAS, SE, lb95, ub95))
}

#Try/show for the third and 77th sample
sampleinfo(samples[,3])
sampleinfo(samples[,77])

#Create empty matrix
results <- matrix(NA,100,5)

#Get all results in one matrix
#loop over samples
for (i in 1:100){
  results[i,] <- sampleinfo(samples[,i])
}

#Store as dataframe
RES <- as.data.frame(results)
RES

#Create a plot that demonstrates the following:
#"A replication of the procedure that generates a 95% confidence interval that is centered around 
#the sample mean would cover the population value at least 95 out of 100 times" (Neyman, 1934)

#Add variable indicating whether 95%CI includes the population value (0)
cover <- RES$V4 < 0 & 0 < RES$V5
cbind(RES,cover)

#A plot showing the mean and 95%CI for all 100 samples, with colour indicating whether the population value is covered
library(ggplot2)
limits <- aes(ymax = RES$V5, ymin = RES$V4)
ggplot(data = RES,
       aes(
         y = V1,
         x = 1:100,
         colour = cover
      )) + 
  geom_hline(aes(yintercept = 0), color = "dark grey", size = 2) + 
  geom_pointrange(limits) + 
  xlab("Simulations 1-100") +
  ylab("Means and 95% Confidence Intervals")

#3 samples do not cover the population value, so it's covered 97 out of 100 times. 

#Present a table containing all simulated samples for which the resulting confidence interval 
#does not contain the population value.
library(tidyverse)
RES %>% filter(cover == FALSE)
