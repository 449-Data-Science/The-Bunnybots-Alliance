library(tidyverse)
library(readr)

rp_map <- c(
    `Red Stocked RP`  = "stocked_rp_red",
    `Red Baked RP`    = "baked_rp_red",
    `Red Dinner RP`   = "dinner_rp_red",
    `Blue Stocked RP` = "stocked_rp_blue",
    `Blue Baked RP`   = "baked_rp_blue",
    `Blue Dinner RP`  = "dinner_rp_blue"
)

process_matches <- function(raw, use_label = FALSE) {
    raw |>
        mutate(
            Match = if (use_label) match_label else as.character(match_number),
            across(c(red1, red2, red3, blue1, blue2, blue3), as.character)
        ) |>
        select(
            Match, red1, red2, red3, blue1, blue2, blue3,
            red_score, blue_score, all_of(unname(rp_map))
        ) |>
        rename(
            `Red 1` = red1, `Red 2` = red2, `Red 3` = red3,
            `Blue 1` = blue1, `Blue 2` = blue2, `Blue 3` = blue3,
            `Red Score` = red_score, `Blue Score` = blue_score,
            all_of(rp_map)
        )
}

process_playoffs <- function(raw) process_matches(raw, use_label = TRUE)

process_rankings <- function(raw) {
    if (nrow(raw) == 0) {
        return(tibble(
            Rank = integer(), Team = character(), `Ranking Score` = double(),
            `Avg Match` = double(), `Record (W-L-T)` = character(),
            DQ = integer(), Played = integer(), `Ranking Total` = integer()
        ))
    }
    
    data <- raw |>
        mutate(
            team_key = gsub("frc", "", team_key),
            ties = matches_played - wins - losses - dq,
            string = paste0(wins, "-", losses, "-", ties),
            `Ranking Score` = `Ranking Total` / (matches_played + dq)
        ) |>
        arrange(desc(`Ranking Score`)) |>
        select(
            rank, team_key, `Ranking Score`, `Avg Match`, string, 
            dq, matches_played, `Ranking Total`
        ) |>
        rename(
            Rank = rank, Team = team_key, `Record (W-L-T)` = string, 
            DQ = dq, Played = matches_played
        )
}

generate_oprs <- function(raw){
    teams <- sort(as.integer(unique(unlist(c(raw[, 2:7])))))
    
    red <- raw |>
        transmute(
            Match, Alliance = "Red", Score = `Red Score`, 
            Team1 = `Red 1`, Team2 = `Red 2`, Team3 = `Red 3`,
        )
    
    blue <- raw |>
        transmute(
            Match, Alliance = "Blue", Score = `Blue Score`,
            Team1 = `Blue 1`, Team2 = `Blue 2`, Team3 = `Blue 3`
        )
    
    data <- bind_rows(red, blue) |>
        arrange(Match, factor(Alliance, levels = c("Red", "Blue")))
    
    lineups <- matrix(
        0, 
        nrow = length(unique(raw$Match)) * 2, 
        ncol = length(unique(unique_teams))
    )
    
    colnames(lineups) <- unique_teams
    for (i in seq_len(nrow(data))) {
        lineups[i, as.character(unlist(data[i, c("Team1", "Team2", "Team3")]))] <- 1
    }
    
    fit <- lm(data$Score ~ 0 + lineups)
    opr <- coef(fit)
    names(opr) <- teams
    sort(opr)
}