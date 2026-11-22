library(dplyr)
library(jsonlite)
library(shiny)

# =============================================================================
# Public data API
#
#   GET /api                                   list of endpoints
#   GET /api/matches/qualification             all qualification matches
#   GET /api/matches/qualification/12          one qualification match
#   GET /api/matches/playoff                   all playoff matches
#   GET /api/matches/playoff/SF4               one playoff match (label or number)
#   GET /api/rankings
#   GET /api/alliances
#   GET /api/teams                             every team seen in the schedule
#   GET /api/teams/449                         that team's matches (quals + playoffs)
#
# Options (query string):  format=json (default) | csv     played=true
#                          slots=true (include raw pantry/haul slot strings)
#
# Everything also works without a path, which is the most portable form:
#   /?api=matches/qualification&format=csv
# =============================================================================

API_TTL <- 5   # seconds a fetched snapshot is reused between requests
api_cache <- new.env()

# Latest arena data, refetched at most every API_TTL seconds so a burst of
# requests from many teams makes one trip to the arena, not one each.
api_arena <- function() {
    now <- as.numeric(Sys.time())
    if (is.null(api_cache$data) || now - api_cache$at > API_TTL) {
        api_cache$data <- fetch_arena()
        api_cache$at <- as.numeric(Sys.time())
    }
    api_cache$data
}

# ---- Routing ----------------------------------------------------------------

api_query <- function(req) shiny::parseQueryString(req$QUERY_STRING %||% "")

# Anything that is not the home page is treated as an API request
is_api_request <- function(req) {
    !identical(req$PATH_INFO, "/") || !is.null(api_query(req)$api)
}

api_parts <- function(req) {
    path <- api_query(req)$api %||% req$PATH_INFO %||% "/"
    path <- gsub("^/+|/+$", "", utils::URLdecode(path))
    parts <- if (nzchar(path)) strsplit(path, "/", fixed = TRUE)[[1]] else character()
    if (length(parts) && parts[1] == "api") parts <- parts[-1]
    parts
}

# ---- Responses --------------------------------------------------------------

api_headers <- list(
    `Access-Control-Allow-Origin` = "*",     # lets a team's website fetch() it
    `Cache-Control` = "public, max-age=5"
)

api_reply <- function(status, content_type, content) {
    shiny::httpResponse(status, content_type, content, headers = api_headers)
}

api_json <- function(x, status = 200L) {
    api_reply(status, "application/json; charset=utf-8",
              jsonlite::toJSON(x, dataframe = "rows", na = "null",
                               auto_unbox = TRUE, digits = NA, pretty = TRUE))
}

api_error <- function(status, message) api_json(list(error = message), status)

api_endpoints <- c(
    "api/matches/qualification", "api/matches/qualification/{number}",
    "api/matches/playoff", "api/matches/playoff/{label}",
    "api/rankings", "api/alliances", "api/teams", "api/teams/{number}"
)

# ---- Data shaping -----------------------------------------------------------

snake <- function(x) tolower(gsub("^_|_$", "", gsub("[^A-Za-z0-9]+", "_", x)))

# Add a `played` flag, optionally keep only played matches, hide the slot
# strings unless asked for
tidy_matches <- function(df, q) {
    df <- mutate(df, played = !is.na(red_score) & !is.na(blue_score), .after = match_label)
    if (identical(tolower(q$played), "true")) df <- filter(df, played)
    if (!identical(tolower(q$slots), "true")) df <- select(df, !starts_with("slots_"))
    df
}

team_matches <- function(quals, playoffs, team, q) {
    all <- bind_rows(
        mutate(quals, type = "qualification", .before = 1),
        mutate(playoffs, type = "playoff", .before = 1)
    )
    on_red  <- all$red1 == team  | all$red2 == team  | all$red3 == team
    on_blue <- all$blue1 == team | all$blue2 == team | all$blue3 == team
    all <- all[which(on_red | on_blue), ]
    if (nrow(all) == 0) return(all)
    all$alliance <- ifelse(all$red1 == team | all$red2 == team | all$red3 == team, "red", "blue")
    all$station <- case_when(
        all$red1 == team | all$blue1 == team ~ 1L,
        all$red2 == team | all$blue2 == team ~ 2L,
        TRUE ~ 3L
    )
    tidy_matches(relocate(all, alliance, station, .after = match_label), q)
}

# Returns list(data = <data frame>, single = <TRUE for one-record answers>)
# or list(error = list(status, message))
api_resource <- function(parts, q, bunny_arena_update) {
    arena <- api_arena()
    err <- function(status, msg) list(error = list(status = status, message = msg))

    head <- if (length(parts)) tolower(parts[1]) else ""

    if (head == "matches") {
        kind <- tolower(parts[2] %||% "")
        source <- switch(kind,
                         qualification = , qualifications = , qual = , quals = arena$matches,
                         playoff = , playoffs = arena$playoffs,
                         return(err(404, "Use matches/qualification or matches/playoff.")))
        df <- tidy_matches(source, q)
        if (length(parts) >= 3) {
            id <- parts[3]
            hit <- df[as.character(df$match_label) == id |
                          (grepl("^[0-9]+$", id) & df$match_number == suppressWarnings(as.integer(id))), ]
            if (nrow(hit) == 0) return(err(404, paste("No match", id, "found.")))
            return(list(data = hit[1, ], single = TRUE))
        }
        return(list(data = df))
    }

    if (head == "rankings") {
        df <- arena$rankings
        if (nrow(df) > 0) {
            names(df) <- snake(names(df))
            df <- mutate(df, team = as.integer(gsub("frc", "", team_key)), .after = team_key)
        }
        return(list(data = df))
    }

    if (head == "alliances") {
        df <- arena$alliances
        names(df) <- snake(names(df))
        return(list(data = df))
    }

    if (head == "teams") {
        all <- bind_rows(arena$matches, arena$playoffs)
        teams <- sort(unique(as.integer(unlist(select(all, matches("^(red|blue)[123]$"))))))
        if (length(parts) >= 2) {
            team <- suppressWarnings(as.integer(parts[2]))
            if (is.na(team) || !team %in% teams) return(err(404, paste("No team", parts[2], "found.")))
            return(list(data = team_matches(arena$matches, arena$playoffs, team, q)))
        }
        return(list(data = tibble(team = teams)))
    }

    err(404, "Unknown endpoint.")
}

api_csv <- function(df) readr::format_csv(df, na = "")

# ---- Entry point ------------------------------------------------------------

api_response <- function(req) {
    q <- api_query(req)
    parts <- api_parts(req)

    if (length(parts) == 0) {
        return(api_json(list(
            name = "The Bunnybots Alliance API",
            endpoints = api_endpoints,
            options = list(format = c("json", "csv"), played = "true", slots = "true"),
            note = "Or use /?api=<endpoint>, e.g. /?api=matches/qualification&format=csv"
        )))
    }

    format <- tolower(q$format %||% "json")
    if (!format %in% c("json", "csv")) return(api_error(400, "format must be json or csv."))

    result <- tryCatch(api_resource(parts, q, bunny_arena_update), error = function(e) {
        message("API error: ", conditionMessage(e))
        list(error = list(status = 500L, message = "Could not read arena data."))
    })
    if (!is.null(result$error)) return(api_error(result$error$status, result$error$message))

    if (format == "csv") {
        return(api_reply(200L, "text/csv; charset=utf-8", api_csv(result$data)))
    }

    if (isTRUE(result$single)) {
        json <- jsonlite::toJSON(result$data, dataframe = "rows", na = "null",
                                 auto_unbox = TRUE, digits = NA, pretty = TRUE)
        # one record: an object rather than a one-item array
        return(api_reply(200L, "application/json; charset=utf-8",
                         jsonlite::prettify(trimws(substr(json, 2, nchar(json) - 1)))))
    }
    api_json(result$data)
}
