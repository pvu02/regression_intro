# Load pop data
library(tidyverse)
population_data <- read_csv("population_data.csv")
glimpse(population_data)

# Get sample
set.seed(1)
sample1_analytic_data <- sample_n(population_data, size=200)
glimpse(sample1_analytic_data)

# Sample regression
sample1_lm_results <- lm(performance ~ IQ, data=sample1_analytic_data)
summary(sample1_lm_results)

library(apaTables)
apa.reg.table(sample1_lm_results)
## b weight = .24, 95% CI [0.19, 0.29]

# population regression
population_lm_results <- lm(performance ~ IQ, data=population_data)
summary(population_lm_results)
## Real slope (b weight) = .20