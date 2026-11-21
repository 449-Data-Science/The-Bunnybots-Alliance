library(shiny)
library(shinycssloaders)
library(shinyWidgets)
library(bslib)

library(tidyverse)
library(DT)
library(ggplot2)

source("R/api-interface.R")
source("R/helper-functions.R")
source("R/table-helpers.R")
source("R/match-details.R")
source("R/api.R")
source("R/api-docs.R")


POLL_INTERVAL <- 5000

last_good <- new.env()

or_last_good <- function(key, fn, empty) {
    tryCatch(
        { value <- fn(); assign(key, value, envir = last_good); value },
        error = function(e) {
            message("arena fetch failed [", key, "]: ", conditionMessage(e))
            get0(key, envir = last_good, inherits = FALSE, ifnotfound = empty)
        }
    )
}

# One snapshot of everything the arena knows. Used by the app (below) and by
# the public API (R/api.R).
fetch_arena <- function() list(
    matches   = or_last_good("matches",  function() get_matches("qualification"), empty_matches()),
    playoffs  = or_last_good("playoffs", function() get_matches("playoff"),       empty_matches()),
    rankings  = or_last_good("rankings", get_rankings,  tibble()),
    alliances = or_last_good("alliances", get_alliances, tibble())
)

arena <- reactivePoll(
    POLL_INTERVAL, session = NULL,
    checkFunc = function() {
        tryCatch(
            rlang::hash(lapply(
                c("/api/matches/qualification", "/api/matches/playoff",
                  "/api/rankings", "/api/alliances"),
                fetch_text
            )),
            error = function(e) "offline"   # keep last good data if arena is down
        )
    },
    valueFunc = function() list(
        matches   = or_last_good("matches",  function() get_matches("qualification"), empty_matches()),
        playoffs  = or_last_good("playoffs", function() get_matches("playoff"),       empty_matches()),
        rankings  = or_last_good("rankings", get_rankings,  tibble()),
        alliances = or_last_good("alliances", get_alliances, tibble())
    )
)