library(dslabs)
data("murders")
murders
library(ggplot2)
library(tidyverse)

murders %>% ggplot(aes(population,total)) + geom_point(alpha=0.5) + geom_smooth(method = "lm")

murder_simulation <- replicate(1000,{sample_n(murders,35) %>% summarize(m = mean(total)) %>% pull(m)})
population_simulation <- replicate(1000,{sample_n(murders,35) %>% summarize(m = mean(population)) %>% pull(m)})

data.frame(population_simulation,murder_simulation) %>% ggplot(aes(population_simulation,murder_simulation)) + geom_point(alpha=0.5) + geom_smooth(method = "lm")
fit <- data.frame(population_simulation,murder_simulation) %>% lm(murder_simulation ~ population_simulation,data=.)
summary(fit)
# very high p-value of 0.2258, population cannot be considered

# even if we consider let'd predict muders in a state with population 90000

predict(fit,data.frame(population_simulation = 90000)) #176.3737
