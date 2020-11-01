## replication script from Assignment 2_Ramljak

library(tidyverse)
library(Hmisc)

set.seed(3) # setting the same seed as in the original study

pop <- tibble(ID = paste0("A", 1:100000), 
              y = rnorm(100000, mean = 40, 10)) %>% 
  mutate(gen.inclusion.prob = 1 / (length(ID))) %>% 
  arrange(y) %>% 
  mutate(rank = percent_rank(y)) %>% 
  mutate(prob.help = case_when(between(percent_rank(y), 0, 0.2) ~ 1, #| # min(y) < y < 1
                               # between(percent_rank(y), 0.9, 1) ~ 1, # 10 < y < max(y)
                               TRUE ~ 0)) %>% 
  mutate(adj.inclusion.prob = case_when(prob.help == 1 ~ 1 / sum(prob.help),
                                        prob.help == 0 ~ 1 / sum(length(ID) - prob.help)))

head(pop)


# 100 SRS without replacement
n.SRS <- map(1:100, ~sample(pop$ID, 1000, replace = T)) %>%
  set_names(paste("Sample", 1:100)) %>% 
  map(as_tibble) %>% 
  map(~select(., ID = value)) %>% 
  map(~mutate(., selected = 1)) %>% 
  map(~right_join(., pop, by = "ID")) %>% 
  map(~mutate(., selected = case_when(is.na(selected) ~ 0,
                                      TRUE ~ selected)))


# 100 Non probability samples without replacement
n.NP <- map(1:100, ~sample(pop$ID, 10000, replace = T, prob = pop$adj.inclusion.prob)) %>%
  set_names(paste("Sample", 1:100)) %>% 
  map(as_tibble) %>% 
  map(~select(., ID = value)) %>% 
  map(~mutate(., selected = 1)) %>% 
  map(~right_join(., pop, by = "ID")) %>% 
  map(~mutate(., selected = case_when(is.na(selected) ~ 0,
                                      TRUE ~ selected)))

# Calculating the statistics
rho.SRS2 <- n.SRS %>% 
  map_df(~summarise(., target.mean = weighted.mean(y, w = selected), 
                    target.sd = sqrt(wtd.var(y, weights = selected)),
                    rho = cor(selected, y))) %>% 
  summarise(SRS.mean.Y = mean(target.mean), 
            SRS.sd.Y = mean(target.sd),
            SRS.rho = mean(rho), 
            SRS.var.rho = var(rho))

rho.NP2 <- n.NP %>% 
  map_df(~summarise(., target.mean = weighted.mean(y, w = selected), 
                    target.sd = sqrt(wtd.var(y, weights = selected)),
                    rho = cor(selected, y))) %>%  
  summarise(NPS.mean.Y = mean(target.mean), 
            NPS.sd.Y = mean(target.sd),
            NPS.rho = mean(rho), 
            NPS.var.rho = var(rho))

rho.SRS2
rho.NP2


# Showcasing the formula

# SRS mean deviation from true mean
round(40.06268 - mean(pop$y), 2)
# filling the results into the formula
round(0.0005904353 * 10 * sqrt((100000 - 1000) / 1000), 2)


#NPS mean deviation from true mean
round(25.93186 - mean(pop$y), 2)
# filling the results into the formula
round(-0.4468931 * sd(pop$y) * sqrt((100000 - 10000) / 10000), 2)
