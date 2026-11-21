navbarPage(
    id = "main_nav",
    title = tagList(
        span(class = "brand-full", "The Bunnybots Alliance: Harvest Havoc 2026"),
        span(class = "brand-short", "The Bunnybots Alliance")
    ),
    theme = bs_theme(
        version = 5, preset = "flatly",
        primary = "#17222d", secondary = "#f0731d",
        base_font = font_google("Barlow", local = FALSE),
        heading_font = font_google("Barlow Condensed", local = FALSE)
    ),
    collapsible = TRUE,
    header = tagList(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
        tags$head(tags$script(src = "script.js", type = "text/javascript"))
    ),
    tabPanel(
        title = "Results",
        
        tags$div(
            class = "results-grid",
            
            # Qualification Matches
            card(
                class = "qualification-card",
                table_header("Qualification Results", "matches_q", "matches_table"),
                fill = FALSE,
                card_body(
                    fillable = FALSE,
                    DTOutput("matches_table")
                )
            ),
            
            # Alliances and playoffs share the right-hand column
            tags$div(
                class = "results-side",
                
                # Alliances
                card(
                    class = "alliances-card",
                    card_header("Alliances"),
                    fill = FALSE,
                    card_body(
                        fillable = FALSE,
                        DTOutput("alliances_table")
                    )
                ),
                
                # Playoffs
                card(
                    class = "playoffs-card",
                    card_header("Playoff Results"),
                    fill = FALSE,
                    card_body(
                        fillable = FALSE,
                        DTOutput("playoffs_table")
                    )
                )
            )
        )
    ),
    tabPanel(
        title = "Rankings",
        card(
            fill = FALSE,
            table_header("Rankings", "rankings_q", "rankings_table"),
            card_body(
                fillable = FALSE,
                DTOutput("rankings_table")
            )
        )
    ),
    tabPanel(
        title = "Matches",
        div(class = "container-fluid", div(class = "row",
        # Sidebar: pick a played match
        div(class = "col-12 col-lg-3",
            div(
                class = "team-picker",
                selectInput(
                    "match_type", label = "Match type",
                    choices = c("Qualification" = "Q", "Playoff" = "P"),
                    selectize = FALSE, width = "100%"
                ),
                virtualSelectInput(
                    "match_pick", label = "Select a match",
                    choices = NULL, multiple = FALSE, search = TRUE
                ),
                p(class = "download-note picker-note", "Only matches that have been played are listed.")
            )
        ),
        div(class = "col-12 col-lg-9 match-main",
            uiOutput("match_heading"),
            uiOutput("match_lineup"),
            div(class = "row g-3",
                div(class = "col-12 col-xl-6",
                    card(
                        fill = FALSE,
                        card_header("Score Breakdown"),
                        card_body(fillable = FALSE, uiOutput("match_breakdown"))
                    )
                ),
                div(class = "col-12 col-xl-6",
                    card(
                        fill = FALSE,
                        card_header("Field View"),
                        card_body(fillable = FALSE, uiOutput("match_graphic"))
                    )
                )
            ),
            card(
                fill = FALSE,
                table_header("All Match Data", "detailed_q", "detailed_table"),
                card_body(fillable = FALSE, DTOutput("detailed_table"))
            )
        )
        ))
    ),
    tabPanel(
        title = "Teams",
        div(class = "container-fluid", div(class = "row",
        div(class = "col-12 col-lg-3", 
            div(
                class = "team-picker",
                virtualSelectInput(
                    "selected_team", label = "Select a team",
                    choices = NULL, multiple = FALSE, search = TRUE
                )
            )
        ),
        div(class = "col-12 col-lg-9",
            card(
                fill = FALSE,
                card_header("Team Matches"),
                card_body(
                    fillable = FALSE,
                    DTOutput("team_table")
                )
            )
        )))
    ),
    tabPanel(
        title = "Insights"
    ),
    tabPanel(
        title = "Scouting",
        card(
            fill = FALSE,
            card_header("Data downloads"),
            card_body(
                fillable = FALSE,
                p(class = "download-note", "Download the current data as CSV files."),
                div(
                    class = "download-grid",
                    downloadButton(
                        outputId = "download_qual_matches",
                        label = "Qualification Matches",
                        icon = icon("download"),
                        class = "btn btn-primary btn-sm"
                    ),
                    downloadButton(
                        outputId = "download_playoffs",
                        label = "Playoff Matches",
                        icon = icon("download"),
                        class = "btn btn-primary btn-sm"
                    ),
                    downloadButton(
                        outputId = "download_rankings",
                        label = "Rankings",
                        icon = icon("download"),
                        class = "btn btn-primary btn-sm"
                    ),
                    downloadButton(
                        outputId = "download_alliances",
                        label = "Alliances",
                        icon = icon("download"),
                        class = "btn btn-primary btn-sm"
                    ),
                    downloadButton(
                        outputId = "download_teams",
                        label = "Teams",
                        icon = icon("download"),
                        class = "btn btn-primary btn-sm"
                    )
                )
            )
        ),
        card(
            fill = FALSE,
            card_header("API"),
            card_body(
                fillable = FALSE,
                uiOutput("api_docs")
            )
        )
    )
)
