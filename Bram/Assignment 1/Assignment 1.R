library(ggplot2)

#set a seed
set.seed(100)

#obtain the samples
mean <- 0
sd <- 1
n <- 10000
nsim <- 100

samples <- replicate(nsim, rnorm(n,mean,sd))

#calculate statistics
means <- colMeans(samples)
bias <- means - 0
se <- sqrt(sd/n)

#calculate 95% CI
df <- n-1
bounds <- qt(.975,df)*se
CI <- data.frame(means-bounds, means+bounds)
CI <- cbind(CI, CI[,1] < 0 & 0 < CI[,2])
colnames(CI) <- c("lower", "upper", "covered")

mean(CI$covered) #95 of the 100 samples include the population mean in the CI

#create plot (based on the solution)
limits <- aes(ymax = CI$upper, ymin = CI$lower)
ggplot(CI, aes(y=means, x=1:100, colour = covered)) + 
  geom_hline(aes(yintercept = 0), color = "grey", size = 1) + 
  geom_pointrange(limits) + 
  xlab("Simulations 1-100") +
  ylab("Means and 95% Confidence Intervals") +
  theme_minimal()

#create table with samples in which the CI does not contain the population value
outliers <- samples[,which(CI$covered == F)]


