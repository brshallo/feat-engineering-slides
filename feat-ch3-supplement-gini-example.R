
library(tidyverse)

inv_logit <- function(x) exp(x) / (1 + exp(x))

df <- tibble(x = inv_logit(c(rnorm(10000, 0, 1), rnorm(5000, .5, 3), rnorm(5000, -.5, 3))),
       description = c(rep("high gini", 10000), rep("low gini", 10000))) 

df %>% 
  ggplot(aes(x = x))+
  geom_histogram()+
  facet_wrap(~description)


gini_calc <- function(p){
  
  mean(p*(1 - p))
}

entropy_calc <- function(p){
  H <- - sum((p*log(p, 2) + (1 - p)*log((1 - p), 2)))
  
  H / (length(p)*2)
}

df %>% 
  group_by(description) %>% 
  summarise(gini = gini_calc(x),
            entropy = entropy_calc(x))
