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

# =========================================================
# OPR (Offensive Power Rating)
#
# Every alliance score is modelled as the sum of its three teams' contributions:
#     metric(alliance) = opr[team1] + opr[team2] + opr[team3]
# and the per-team contributions are found by ordinary least squares, exactly
# like TheBlueAlliance. The same fit works for any alliance-level number, so
# the score, the oven, the pantry levels etc. all go through one function.
# =========================================================

# Metrics that are always shown. Names are the table headers, values are the
# columns produced by alliance_stats().
opr_core <- c(
    `Score`            = "score",
    `Carrots (Oven)`   = "carrots_oven",
    `Cakes (Oven)`     = "cakes_oven",
    `Pieces L1`        = "pieces_l1",
    `Pieces L2`        = "pieces_l2",
    `Pieces L3`        = "pieces_l3",
    `Cakes (Pantry)`   = "cakes_pantry",
    `Carrots (Pantry)` = "carrots_pantry"
)

# Metrics the user can pick as the extra column, grouped for the dropdown.
opr_extra <- list(
    `Pantry shelves` = c(
        `Carrots L1` = "carrots_l1", `Carrots L2` = "carrots_l2", `Carrots L3` = "carrots_l3",
        `Cakes L1`   = "cakes_l1",   `Cakes L2`   = "cakes_l2",   `Cakes L3`   = "cakes_l3"
    ),
    `Auto` = c(`Robots taxied` = "taxi"),
    `Endgame` = c(
        `Robots parked` = "park",
        `Carrots hauled` = "carrot_haul", `Cakes hauled` = "cake_haul"
    ),
    `Fouls` = c(`Minor fouls` = "minor", `Major fouls` = "major"),
    `Ranking points` = c(
        `Stocked RP` = "stocked_rp", `Baked RP` = "baked_rp", `Dinner RP` = "dinner_rp"
    )
)
# Same thing without the groups: label -> column
opr_extra_flat <- unlist(unname(opr_extra))

# One row per alliance per match (so two rows per match), with the alliance-level
# numbers OPR is fitted to. `raw` is the wide one-row-per-match data
# (the shape of matches.csv / flatten_match()).
alliance_stats <- function(raw) {
    side <- function(colour) {
        k <- if (colour == "red") "R" else "B"
        col <- function(name) raw[[paste0(name, "_", colour)]]
        # Per-robot columns (taxi_R1..R3 etc.) summed over the alliance
        robots <- function(name) {
            Reduce(`+`, lapply(1:3, function(i) as.numeric(raw[[paste0(name, "_", k, i)]])))
        }
        
        tibble(
            Match = raw$match_number,
            Alliance = colour,
            Team1 = raw[[paste0(colour, "1")]],
            Team2 = raw[[paste0(colour, "2")]],
            Team3 = raw[[paste0(colour, "3")]],
            
            score        = raw[[paste0(colour, "_score")]],
            carrots_oven = col("carrot_oven"),
            cakes_oven   = col("cake_oven"),
            carrots_l1 = col("carrot_L1"), carrots_l2 = col("carrot_L2"), carrots_l3 = col("carrot_L3"),
            cakes_l1   = col("cake_L1"),   cakes_l2   = col("cake_L2"),   cakes_l3   = col("cake_L3"),
            
            taxi = robots("taxi"), park = robots("park"),
            carrot_haul = robots("carrot_haul"), cake_haul = robots("cake_haul"),
            minor = col("minor"), major = col("major"),
            stocked_rp = as.numeric(col("stocked_rp")),
            baked_rp   = as.numeric(col("baked_rp")),
            dinner_rp  = as.numeric(col("dinner_rp"))
        )
    }
    
    bind_rows(side("red"), side("blue")) |>
        mutate(
            pieces_l1 = carrots_l1 + cakes_l1,
            pieces_l2 = carrots_l2 + cakes_l2,
            pieces_l3 = carrots_l3 + cakes_l3,
            carrots_pantry = carrots_l1 + carrots_l2 + carrots_l3,
            cakes_pantry   = cakes_l1 + cakes_l2 + cakes_l3
        ) |>
        arrange(Match, factor(Alliance, levels = c("red", "blue")))
}

# Least-squares solution of A x = Y using the pseudo-inverse. When the schedule
# is complete this is identical to ordinary least squares; early in an event
# (fewer matches than teams, or teams that have only played together) lm() would
# return NA for some teams, while this still gives the minimum-norm fit.
solve_least_squares <- function(A, Y, tol = 1e-8) {
    decomposition <- svd(A)
    keep <- decomposition$d > tol * max(decomposition$d)
    decomposition$v[, keep, drop = FALSE] %*%
        (crossprod(decomposition$u[, keep, drop = FALSE], Y) / decomposition$d[keep])
}

# OPR of every team for each metric in `metrics` (a named vector like opr_core).
# Returns one row per team: Team, Matches (alliances they appear in), then one
# column per metric, named by the metric's label. Matches that haven't been
# played yet (NA values) are ignored.
compute_oprs <- function(stats, metrics) {
    team_cols <- c("Team1", "Team2", "Team3")
    
    played <- stats |>
        filter(if_all(all_of(c(team_cols, unname(metrics))), ~ !is.na(.x)))
    
    if (nrow(played) == 0) {
        empty <- tibble(Team = integer(), Matches = integer())
        for (label in names(metrics)) empty[[label]] <- double()
        return(empty)
    }
    
    teams <- sort(unique(unlist(played[team_cols])))
    
    # Design matrix: one row per alliance, a 1 under each of its three teams
    A <- matrix(0, nrow = nrow(played), ncol = length(teams))
    for (column in team_cols) {
        A[cbind(seq_len(nrow(played)), match(played[[column]], teams))] <- 1
    }
    
    Y <- sapply(unname(metrics), function(m) as.numeric(played[[m]]))
    Y <- matrix(Y, nrow = nrow(played), dimnames = list(NULL, names(metrics)))
    
    coefficients <- solve_least_squares(A, Y)
    colnames(coefficients) <- names(metrics)
    
    bind_cols(
        tibble(Team = as.integer(teams), Matches = as.integer(colSums(A))),
        as_tibble(coefficients)
    )
}