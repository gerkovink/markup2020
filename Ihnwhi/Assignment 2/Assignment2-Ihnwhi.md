---
title: "Markup Languages and Reproducible Programming in Statistics"
subtitle: Week 2 Exercise
author:
    - "[Ihnwhi Heo](https://ihnwhiheo.github.io/)"
    - Department of Methodology and Statistics, Utrecht University
date: 'Last modified: 23
  October 2020'
output:
  html_document:
    keep_md: true
    theme: flatly
    highlight: tango
    toc: true
    toc_float: true
---



## 1. Does something that requires RNG

Imagine Ihnwhi wants to travel to the United States. Since he is open to all states, he decides to let R give him suggestions concerning where to go. To that end, Ihnwhi lets R to name five states.


```r
# Let R name random five states in the USA.
sample(state.name, 5)
```

```
## [1] "Oklahoma"    "Oregon"      "Hawaii"      "Mississippi" "Vermont"
```
## 2. Fixes the RNG seed

However, Ihnwhi finds out that R is a bit capricious (i.e., grillig in Nederlands). Therefore, he decides to give R a seed value not to let it be capricious.

If you can guess the meaning of the seed value below, contact [`Ihnwhi`](mailto:ihnwhi.heo@gmail.com)! He will send you a present.` I am quite sure nobody will see this statement, though!


```r
# Set a seed value for reproducibility
set.seed(322)

# Let R name random five states in the USA.
sample(state.name, 5)
```

```
## [1] "California"  "Connecticut" "Tennessee"   "Alaska"      "Alabama"
```

All right, Out of the five suggestions, Ihnwhi determines to travel to California since he loves the beach!

## 3. Replicates the results

Ihnwhi, a curious guy, changes his mind. He decides to make a data frame that contains R's suggestions by iterating 100 times.


```r
# Set a seed value for reproducibility
set.seed(322)

# Create a function that lets R suggest five US states
suggest.ihnwhi.usa.states <- function(N) {
  recommendation <- sample(state.name, N)
  return(recommendation)}

# Create an empty matrix to store R's suggestions
recom.matrix <- matrix(NA, nrow=100, ncol=5)

# Iterate R's suggestion algorithm by 100 times
recom.initial <- replicate(100, suggest.ihnwhi.usa.states(5), simplify = FALSE)
for (i in 1:100) {
  recom.matrix[i,] <- recom.initial[[i]]
}

# The first six rows of the matrix of R's recommendations
head(recom.matrix)
```

```
##      [,1]            [,2]           [,3]        [,4]         [,5]           
## [1,] "California"    "Connecticut"  "Tennessee" "Alaska"     "Alabama"      
## [2,] "New Hampshire" "Alabama"      "Missouri"  "California" "Wisconsin"    
## [3,] "Mississippi"   "Wisconsin"    "Missouri"  "Ohio"       "Arkansas"     
## [4,] "Georgia"       "Illinois"     "Arkansas"  "Kansas"     "West Virginia"
## [5,] "Utah"          "North Dakota" "Kentucky"  "Maryland"   "Pennsylvania" 
## [6,] "Alabama"       "Illinois"     "Kansas"    "Nebraska"   "Oregon"
```

## 4. Communicates the info of your session

If you want to communicate with me sincerely, do not see the output below but contact [`Ihnwhi`](mailto:ihnwhi.heo@gmail.com)! He is waiting for your suggestions!


```r
# Print the session information
sessionInfo()
```

```
## R version 3.6.3 (2020-02-29)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows 10 x64 (build 19041)
## 
## Matrix products: default
## 
## locale:
## [1] LC_COLLATE=Korean_Korea.949        LC_CTYPE=Korean_Korea.949         
## [3] LC_MONETARY=Korean_Korea.949       LC_NUMERIC=C                      
## [5] LC_TIME=English_United States.1252
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## loaded via a namespace (and not attached):
##  [1] compiler_3.6.3  magrittr_1.5    tools_3.6.3     htmltools_0.5.0
##  [5] yaml_2.2.1      stringi_1.4.6   rmarkdown_2.3   knitr_1.30     
##  [9] stringr_1.4.0   xfun_0.18       digest_0.6.25   rlang_0.4.7    
## [13] evaluate_0.14
```

