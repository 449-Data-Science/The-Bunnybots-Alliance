rm(list = ls())
library(truncnorm)

set.seed(9449) 

num_teams <- 25
teams <- c(
    321, 449, 9449, 526, 611, 614, 686, 1599, 2106, 2377, 2537, 2538, 3504, 
    4472, 4575, 4821, 5115, 5243, 5338, 5353, 5549, 5587, 8230, 8726, 9033
)

values <- rtruncnorm(num_teams, a = 0, b = 100, mean = 50, sd = 25)
names(values) <- teams

save(values, file = "simulation/data/values.rda")
