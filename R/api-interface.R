library(httr2)
library(jsonlite)
library(dplyr)

cheesy_base_url <- Sys.getenv("ARENA_URL", "http://10.0.100.5:8080")

fetch_text <- function(path) {
    request(paste0(cheesy_base_url, path)) |>
        req_timeout(3) |>
        req_perform() |>
        resp_body_string()
}

# One match from the arena JSON -> one row in the shape process_matches() expects.
# This is the ONLY place that knows about the game's field names.
flatten_match <- function(m) {
    red  <- m$Result$RedSummary
    blue <- m$Result$BlueSummary
    red_raw  <- m$Result$RedScore
    blue_raw <- m$Result$BlueScore
    flag <- function(summary, name) if (is.null(summary)) NA else isTRUE(summary[[name]])
    team <- function(x) if (is.null(x) || x == 0) NA_integer_ else as.integer(x)
    
    shelf_count <- function(raw, shelf_index, value) {
        shelves <- raw$Pantry$PantryShelves
        if (is.null(shelves) || length(shelves) < shelf_index) return(NA_integer_)
        sum(unlist(shelves[[shelf_index]]) == value)
    }
    harvest_count <- function(raw, robot_index, value) {
        rows <- raw$EndgameHarvest
        if (is.null(rows) || length(rows) < robot_index) return(NA_integer_)
        sum(unlist(rows[[robot_index]]) == value)
    }
    # The raw slot contents ("1,0,2,..."), kept so the match graphic can draw
    # which slot holds what. 1 = carrot, 2 = cake, anything else = empty.
    slot_string <- function(slots) {
        if (is.null(slots)) return(NA_character_)
        paste(unlist(slots), collapse = ",")
    }
    pantry_slots <- function(raw, shelf_index) {
        shelves <- raw$Pantry$PantryShelves
        if (is.null(shelves) || length(shelves) < shelf_index) return(NA_character_)
        slot_string(shelves[[shelf_index]])
    }
    haul_slots <- function(raw, robot_index) {
        rows <- raw$EndgameHarvest
        if (is.null(rows) || length(rows) < robot_index) return(NA_character_)
        slot_string(rows[[robot_index]])
    }
    taxi <- function(raw, i) if (is.null(raw$AutoTaxi)) NA else isTRUE(raw$AutoTaxi[[i]])
    park <- function(raw, i) if (is.null(raw$EndgamePark)) NA else isTRUE(raw$EndgamePark[[i]])
    fouls_count <- function(raw, major) {
        if (length(raw$Fouls) == 0) return(0L)
        sum(vapply(raw$Fouls, function(f) isTRUE(f$IsMajor) == major, logical(1)))
    }
    
    tibble(
        match_number = m$TypeOrder,
        match_label  = m$ShortName,
        red1  = team(m$Red1),  red2  = team(m$Red2),  red3  = team(m$Red3),
        blue1 = team(m$Blue1), blue2 = team(m$Blue2), blue3 = team(m$Blue3),
        
        taxi_R1 = taxi(red_raw, 1), taxi_R2 = taxi(red_raw, 2), taxi_R3 = taxi(red_raw, 3),
        taxi_B1 = taxi(blue_raw, 1), taxi_B2 = taxi(blue_raw, 2), taxi_B3 = taxi(blue_raw, 3),
        
        carrot_oven_red = red_raw$OvenCarrot %||% NA_integer_,
        cake_oven_red   = red_raw$OvenCarrotCake %||% NA_integer_,
        carrot_L1_red = shelf_count(red_raw, 1, 1), carrot_L2_red = shelf_count(red_raw, 2, 1), carrot_L3_red = shelf_count(red_raw, 3, 1),
        cake_L1_red   = shelf_count(red_raw, 1, 2), cake_L2_red   = shelf_count(red_raw, 2, 2), cake_L3_red   = shelf_count(red_raw, 3, 2),
        
        carrot_oven_blue = blue_raw$OvenCarrot %||% NA_integer_,
        cake_oven_blue   = blue_raw$OvenCarrotCake %||% NA_integer_,
        carrot_L1_blue = shelf_count(blue_raw, 1, 1), carrot_L2_blue = shelf_count(blue_raw, 2, 1), carrot_L3_blue = shelf_count(blue_raw, 3, 1),
        cake_L1_blue   = shelf_count(blue_raw, 1, 2), cake_L2_blue   = shelf_count(blue_raw, 2, 2), cake_L3_blue   = shelf_count(blue_raw, 3, 2),
        
        park_R1 = park(red_raw, 1), park_R2 = park(red_raw, 2), park_R3 = park(red_raw, 3),
        park_B1 = park(blue_raw, 1), park_B2 = park(blue_raw, 2), park_B3 = park(blue_raw, 3),
        
        carrot_haul_R1 = harvest_count(red_raw, 1, 1), carrot_haul_R2 = harvest_count(red_raw, 2, 1), carrot_haul_R3 = harvest_count(red_raw, 3, 1),
        cake_haul_R1   = harvest_count(red_raw, 1, 2), cake_haul_R2   = harvest_count(red_raw, 2, 2), cake_haul_R3   = harvest_count(red_raw, 3, 2),
        carrot_haul_B1 = harvest_count(blue_raw, 1, 1), carrot_haul_B2 = harvest_count(blue_raw, 2, 1), carrot_haul_B3 = harvest_count(blue_raw, 3, 1),
        cake_haul_B1   = harvest_count(blue_raw, 1, 2), cake_haul_B2   = harvest_count(blue_raw, 2, 2), cake_haul_B3   = harvest_count(blue_raw, 3, 2),
        
        minor_red  = fouls_count(red_raw, FALSE),  major_red  = fouls_count(red_raw, TRUE),
        minor_blue = fouls_count(blue_raw, FALSE), major_blue = fouls_count(blue_raw, TRUE),
        
        stocked_rp_red  = flag(red,  "StockedUpRankingPoint"),
        baked_rp_red    = flag(red,  "BakedUpRankingPoint"),
        dinner_rp_red   = flag(red,  "DinnerRankingPoint"),
        stocked_rp_blue = flag(blue, "StockedUpRankingPoint"),
        baked_rp_blue   = flag(blue, "BakedUpRankingPoint"),
        dinner_rp_blue  = flag(blue, "DinnerRankingPoint"),
        
        red_score  = red$Score  %||% NA_integer_,
        blue_score = blue$Score %||% NA_integer_,
        
        # Slot-level detail for the match graphic (see slot_string() above)
        slots_L1_red  = pantry_slots(red_raw, 1),  slots_L2_red  = pantry_slots(red_raw, 2),  slots_L3_red  = pantry_slots(red_raw, 3),
        slots_L1_blue = pantry_slots(blue_raw, 1), slots_L2_blue = pantry_slots(blue_raw, 2), slots_L3_blue = pantry_slots(blue_raw, 3),
        slots_haul_R1 = haul_slots(red_raw, 1),  slots_haul_R2 = haul_slots(red_raw, 2),  slots_haul_R3 = haul_slots(red_raw, 3),
        slots_haul_B1 = haul_slots(blue_raw, 1), slots_haul_B2 = haul_slots(blue_raw, 2), slots_haul_B3 = haul_slots(blue_raw, 3)
    )
}

empty_matches <- function() {
    tibble(
        match_number = integer(), match_label = character(),
        red1 = integer(), red2 = integer(), red3 = integer(),
        blue1 = integer(), blue2 = integer(), blue3 = integer(),
        taxi_R1 = logical(), taxi_R2 = logical(), taxi_R3 = logical(),
        carrot_oven_red = integer(), carrot_L1_red = integer(), carrot_L2_red = integer(), carrot_L3_red = integer(),
        cake_oven_red = integer(), cake_L1_red = integer(), cake_L2_red = integer(), cake_L3_red = integer(),
        park_R1 = logical(), park_R2 = logical(), park_R3 = logical(),
        carrot_haul_R1 = integer(), carrot_haul_R2 = integer(), carrot_haul_R3 = integer(),
        cake_haul_R1 = integer(), cake_haul_R2 = integer(), cake_haul_R3 = integer(),
        minor_red = integer(), major_red = integer(),
        taxi_B1 = logical(), taxi_B2 = logical(), taxi_B3 = logical(),
        carrot_oven_blue = integer(), carrot_L1_blue = integer(), carrot_L2_blue = integer(), carrot_L3_blue = integer(),
        cake_oven_blue = integer(), cake_L1_blue = integer(), cake_L2_blue = integer(), cake_L3_blue = integer(),
        park_B1 = logical(), park_B2 = logical(), park_B3 = logical(),
        carrot_haul_B1 = integer(), carrot_haul_B2 = integer(), carrot_haul_B3 = integer(),
        cake_haul_B1 = integer(), cake_haul_B2 = integer(), cake_haul_B3 = integer(),
        minor_blue = integer(), major_blue = integer(),
        stocked_rp_red = logical(), baked_rp_red = logical(), dinner_rp_red = logical(),
        stocked_rp_blue = logical(), baked_rp_blue = logical(), dinner_rp_blue = logical(),
        red_score = integer(), blue_score = integer(),
        slots_L1_red = character(), slots_L2_red = character(), slots_L3_red = character(),
        slots_L1_blue = character(), slots_L2_blue = character(), slots_L3_blue = character(),
        slots_haul_R1 = character(), slots_haul_R2 = character(), slots_haul_R3 = character(),
        slots_haul_B1 = character(), slots_haul_B2 = character(), slots_haul_B3 = character()
    )
}

get_matches <- function(match_type = "qualification") {
    raw <- fetch_text(paste0("/api/matches/", match_type)) |>
        jsonlite::fromJSON(simplifyVector = FALSE)
    # bind_rows() keeps the columns even when `raw` is an empty list
    bind_rows(empty_matches(), purrr::map_dfr(raw, flatten_match))
}

get_schedule <- function(match_type = "qualification") {
    req <- request(paste0(cheesy_base_url, "/reports/csv/schedule/", match_type))
    resp <- req_perform(req)
    raw <- resp_body_string(resp) |> 
        read_csv()
}

get_rankings <- function() {
    resp <- request(paste0(cheesy_base_url, "/api/rankings")) |> req_perform()
    raw <- resp_body_json(resp, simplifyVector = TRUE)
    as_tibble(raw$Rankings) |>
        rename(
            rank = Rank, team_key = TeamId, `Ranking Total` = RankingPoints,
            `Avg Match` = MatchPoints, wins = Wins, losses = Losses, 
            dq = Disqualifications, matches_played = Played
        )
}

get_alliances <- function() {
    req <- request(paste0(cheesy_base_url, "/api/alliances"))
    resp <- req_perform(req)
    raw <- resp_body_json(resp, simplifyVector = FALSE)

    out <- purrr::map_dfr(raw, function(m) {
        ids <- c(unlist(m$TeamIds), rep(NA, 3))[1:3]   # pad if fewer than 3 picks
        ids[!is.na(ids) & ids == 0] <- NA
        tibble(
            Alliance = paste0("Alliance ", m$Id),
            Captain = ids[1],
            `First Pick` = ids[2],
            `Second Pick` = ids[3]
        )
    })

    bind_rows(
        tibble(
            Alliance = character(), Captain = integer(),
            `First Pick` = integer(), `Second Pick` = integer()
        ),
        out
    )
}