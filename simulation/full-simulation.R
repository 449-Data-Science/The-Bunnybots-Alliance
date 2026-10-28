rm(list = ls())

source("simulation/1. creating-teams.R")
source("simulation/2. creating-schedule.R")
source("simulation/3. creating-matches.R")
source("simulation/4. creating-rankings.R")
source("simulation/5. creating-playoffs.R")

# Schedule is predetermined via the MatchMaker algorithm, found in matches.txt