rm(list = ls())
library(tidyverse)
library(data.table)

load("simulation/data/values.rda")

# MatchMaker -t 25 -r 8 -g
raw <- fread("simulation/data/matches-numerical.txt")

data <- raw |>
    select(!V1) |>
    rename(R1 = V2, R2 = V3, R3 = V4, B1 = V5, B2 = V6, B3 = V7) |>
    mutate(across(everything(), ~ as.integer(gsub("\\D", "", as.character(.x)))))

data$match <- 1:length(data$R1)
data <- data |> select(match, everything())

data <- data |>
    mutate(across(everything() & !match, ~ as.integer(names(values[.x]))))

schedule <- data

save(schedule, file = "simulation/data/schedule.rda")
