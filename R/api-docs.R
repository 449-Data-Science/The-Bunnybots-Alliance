library(htmltools)

# =============================================================================
# The API documentation card on the Scouting tab (same content as API.md).
# `base` is the app's own address, so every example URL can be copied as-is.
#
# Text is written as plain strings: `code` in backticks, **bold**, and BASE for
# the app's address. They are turned into one HTML string per paragraph, which
# keeps inline code from picking up stray whitespace.
# =============================================================================

# Address of the running app from Shiny's session$clientData, e.g.
# "https://youraccount.shinyapps.io/bunnybots". Falls back to a placeholder.
api_base_url <- function(client_data) {
    host <- client_data$url_hostname
    if (is.null(host) || !nzchar(host)) return("https://youraccount.shinyapps.io/yourapp")
    port <- client_data$url_port
    port <- if (is.null(port) || !nzchar(port)) "" else paste0(":", port)
    path <- sub("/+$", "", client_data$url_pathname %||% "")
    paste0(client_data$url_protocol %||% "https:", "//", host, port, path)
}

api_docs_ui <- function(base) {
    # "`x` and **y**" -> HTML, with BASE replaced by the app's address
    md <- function(s) {
        s <- gsub("BASE", base, s, fixed = TRUE)
        s <- htmlEscape(s)
        s <- gsub("`([^`]+)`", "<code>\\1</code>", s)
        s <- gsub("\\*\\*([^*]+)\\*\\*", "<strong>\\1</strong>", s)
        HTML(s)
    }
    p_  <- function(s, ...) tags$p(md(s), ...)
    h_  <- function(s) tags$h4(s)
    # A fenced block: raw text, one string, no layout whitespace
    block <- function(s) {
        s <- htmlEscape(gsub("BASE", base, s, fixed = TRUE))
        HTML(paste0('<pre class="api-code"><code>', s, "</code></pre>"))
    }
    table_of <- function(header, rows) {
        div(class = "api-table-wrap",
            tags$table(
                class = "tba-table zebra api-table",
                tags$thead(tags$tr(lapply(header, tags$th))),
                tags$tbody(lapply(rows, function(r) tags$tr(lapply(r, function(cell) tags$td(md(cell))))))
            ))
    }

    div(
        class = "api-docs",
        p_(paste0(
            "Read-only JSON / CSV access to the same data the site shows. 
            No key needed! But please note that the actual base will be 
            different at Bunnybots—I would recommend having the base be 
            alterable in your code. The current base is: BASE."
        )),
        p_(paste0(
            "The server may also take a couple seconds to spin up. 
            Email 449datascience@gmail.com with any questions!"
        )),

        h_("Endpoints"),
        table_of(
            c("URL", "Returns"),
            list(
                c("`BASE/api`", "list of endpoints"),
                c("`BASE/api/matches/qualification`", "every qualification match"),
                c("`BASE/api/matches/qualification/12`", "qualification match 12"),
                c("`BASE/api/matches/playoff`", "every playoff match"),
                c("`BASE/api/matches/playoff/M4`", "one playoff match (by label, e.g. `M4`, or by number)"),
                c("`BASE/api/rankings`", "rankings"),
                c("`BASE/api/alliances`", "alliances"),
                c("`BASE/api/teams`", "every team in the schedule"),
                c("`BASE/api/teams/449`",
                  "all of team 449's matches (qualification + playoff)")
            )
        ),
        p_("The `/api` prefix is optional: `BASE/matches/qualification` works too."),
	br(),

        h_("Options (add to the URL)"),
        table_of(
            c("Option", "Effect"),
            list(
                c("`?format=csv`", "CSV instead of JSON (default is `json`)"),
                c("`?played=true`", "only matches that have been played"),
                c("`?slots=true`", "include the raw pantry / haul slot strings (`slots_*` columns)")
            )
        ),
        p_("Combine with `&`, e.g. `BASE/api/matches/qualification?played=true&format=csv`."),
	br(),

        h_("If the path form doesn't work on your host"),
        p_("Every endpoint is also available with no path at all, which works on any host:"),
        block("BASE/?api=matches/qualification&format=csv\nBASE/?api=teams/449"),
	br(),

        h_("Match fields"),
        p_(paste0(
            "Same columns as the app's tables: `match_number`, `match_label`, `played`, `red1..3`, `blue1..3`, ",
            "taxi / park per robot (`taxi_R1`, `park_B2`, ...), ",
            "pantry counts per level (`carrot_L1_red`, `cake_L3_blue`, ...), ",
            "oven counts, hauls per robot (`carrot_haul_R1`, ...), ",
            "fouls (`minor_red`, `major_blue`, ...), ",
            "the three ranking-point flags per alliance, and `red_score` / `blue_score`. ",
            "Unplayed matches have `null` scores.")),
	br(),

        h_("Examples"),
        p_("Python", class = "api-example-label"),
        block(paste0('import requests\n',
                     'quals = requests.get("BASE/api/matches/qualification",\n',
                     '                     params={"played": "true"}).json()')),
        p_("R", class = "api-example-label"),
        block(paste0('quals <- jsonlite::fromJSON("BASE/api/matches/qualification?played=true")\n',
                     'ranks <- readr::read_csv("BASE/api/rankings?format=csv")')),
        p_("JavaScript", class = "api-example-label"),
        block('const r = await fetch("BASE/api/teams/449");\nconst games = await r.json();'),
        p_("Google Sheets", class = "api-example-label"),
        block('=IMPORTDATA("BASE/api/rankings?format=csv")'),
	br(),

        h_("Errors"),
        p_(paste0("Errors are JSON with an `error` message: `404` unknown endpoint, match or team; ",
                  "`400` bad format; `500` the arena could not be read."))
    )
}
