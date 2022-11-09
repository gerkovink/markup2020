# Monte carlo simulatie opdracht
# Step 1: simulate 100 samples, each containing 200 scores, drawn from the standard normal distibution
library(tidyverse)
set.seed(100)
samples<-replicate(100,rnorm(5000,mean = 0,sd=1))
# Step 2: calculate error(the mean difference fom 0)
means<-apply(samples,2,mean)
# Calculate SE
SE<-1/sqrt(5000)

# Calculate intervals
# FIrst we need degrees of freedom
intervallow<-qnorm(0.025,mean=means,sd=1) * SE
intervalhigh<-qnorm(0.975,mean=means,sd=1) * SE
upperlimit<-means+intervalhigh
lowerlimit<-means+intervallow

# Place all this nice information in a single dataframe
data<-as.data.frame(cbind(means,lowerlimit,upperlimit))

# Provide an indicator of whether the confidence interval include the true population mean
data$zeroinconf<-0 # Create some space
for( i in 1:length(data[,1])){
  if(data$lowerlimit[i]<0 & data$upperlimit[i]>0){
    data$zeroinconf[i]<-1
  }else
    data$zeroinconf[i]<-0
}
# Plot it (ik vondt de in het antwoord gebruikte plot zo mooi dat ik hem zelf oo een keer zou maken:))
limits <- aes(ymax = data$upperlimit, ymin = data$lowerlimit)
ggplot(data, aes(y=means, x=1:100, colour = zeroinconf)) + 
  geom_hline(aes(yintercept = 0), color = "dark grey", size = 2) + 
  geom_pointrange(limits) + 
  xlab("Simulations 1-100") +
  ylab("Means and 95% Confidence Intervals")

# Show table!
dat<-data%>%
  filter(zeroinconf==0)
