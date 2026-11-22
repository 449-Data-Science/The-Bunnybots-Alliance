function(input, output, session) {
    if (bunny_arena_update) {
        matches_raw   <- reactive(arena()$matches)
        playoffs_raw  <- reactive(arena()$playoffs)
        rankings_raw  <- reactive(arena()$rankings)
        alliances_raw <- reactive(arena()$alliances)
    
        matches_data  <- reactive(process_matches(matches_raw()))
        rankings_data <- reactive(process_rankings(rankings_raw()))
        playoffs_data <- reactive(process_playoffs(playoffs_raw()))
    } else {
        matches_raw <- reactiveVal(read_csv("data/matches.csv"))
        rankings_raw <- reactiveVal(read_csv("data/rankings.csv"))
        playoffs_raw <- reactiveVal(read_csv("data/playoffs.csv"))
        alliances_raw <- reactiveVal(read_csv("data/alliances.csv"))
        
        matches_data <- reactiveVal()
        rankings_data <- reactiveVal()
        playoffs_data <- reactiveVal()
        
        observe({
            matches_data(process_matches(matches_raw()))
            rankings_data(process_rankings(rankings_raw()))
            playoffs_data(process_playoffs(playoffs_raw()))
        })   
    }
    
    # OPR is fitted on qualification matches only, like TheBlueAlliance. One
    # solve covers every metric, so changing the extra column never refits.
    oprs_all <- reactive(
        compute_oprs(alliance_stats(matches_raw()), c(opr_core, opr_extra_flat))
    )
    
    team_choices <- reactive({
        matches_data() |>
            select(matches("^(Red|Blue) [123]$")) |>
            unlist() |> as.integer() |> unique() |> sort()
    })
    
    observe({
        updateVirtualSelect(
            "selected_team", 
            choices = team_choices(), 
            selected = isolate(input$selected_team)
            )
    })
    
    # A click on a team number anywhere opens that team on the Teams tab
    observeEvent(input$goto_team, {
        team <- trimws(input$goto_team$team)
        if (!team %in% as.character(team_choices())) return()
        updateVirtualSelect("selected_team", selected = team)
        updateNavbarPage(session, "main_nav", selected = "Teams")
    })
    
    # ---- Search boxes ----
    # Filtering happens in the browser (www/script.js). This reads the current
    # search text without triggering a redraw, so a table that refreshes with
    # new data keeps its filter.
    query_of <- function(id) {
        value <- isolate(input[[id]])
        if (is.null(value)) "" else value
    }
    
    output$matches_table  <- renderDT({
        match_table(matches_data(), query = query_of("matches_q"))
    })
    
    output$playoffs_table <- renderDT({
        match_table(playoffs_data())
    })
    
    output$alliances_table <- renderDT({
        dataframe <- alliances_raw()
        # Same headers whether the data comes from the simulation or the API
        names(dataframe) <- tools::toTitleCase(gsub("_", " ", names(dataframe)))
        simple_table(dataframe)
    })
    
    output$rankings_table <- renderDT({
        rankings_table(rankings_data(), query = query_of("rankings_q"))
    })
    
    output$opr_table <- renderDT({
        req(input$opr_extra)
        extra <- names(opr_extra_flat)[match(input$opr_extra, opr_extra_flat)]
        
        dataframe <- oprs_all() |>
            select(Team, Matches, all_of(names(opr_core)), all_of(extra))
        
        opr_table(dataframe, extra = extra, query = query_of("opr_q"))
    })
    
    output$detailed_table  <- renderDT({
        # Slot strings are for the graphic; they would only clutter this table
        dataframe <- bind_rows(matches_raw(), playoffs_raw()) |>
            select(!starts_with("slots_"))
        simple_table(dataframe, query = query_of("detailed_q"))
    })
    
    output$team_table <- renderDT({
        req(input$selected_team)
        team <- as.character(input$selected_team)
        
        dataframe <- rbind(matches_data(), playoffs_data()) |>
            filter(
                `Red 1` == team | `Red 2` == team | `Red 3` == team |
                `Blue 1` == team | `Blue 2` == team | `Blue 3` == team
            )
        
        match_table(dataframe, highlight = team)
    })
    
    # ---- Match Details tab ----
    catalog <- reactive(match_catalog(matches_raw(), playoffs_raw()))
    played_catalog <- reactive(filter(catalog(), played))
    
    # A click on a Match cell asks for a specific match. The request waits here
    # until the dropdowns have caught up (switching type refills the match list).
    wanted <- reactiveValues(type = NULL, key = NULL, tick = 0)
    
    observeEvent(input$goto_match, {
        label <- trimws(input$goto_match$label)
        type  <- if (grepl("^[0-9]+$", label)) "Q" else "P"
        hit   <- filter(played_catalog(), .data$type == !!type, .data$key == !!label)
        if (nrow(hit) == 0) {
            showNotification("That match hasn't been played yet.", type = "message", duration = 3)
            return()
        }
        wanted$type <- type
        wanted$key  <- label
        wanted$tick <- wanted$tick + 1
        updateSelectInput(session, "match_type", selected = type)
        updateNavbarPage(session, "main_nav", selected = "Matches")
    })
    
    # Refill the match list (played matches of the chosen type only), keeping
    # the current choice unless a click asked for another one.
    observe({
        wanted$tick
        sel_type <- input$match_type
        if (is.null(sel_type)) sel_type <- "Q"
        available <- filter(played_catalog(), .data$type == sel_type)
        
        keep <- isolate(input$match_pick)
        want_key  <- isolate(wanted$key)
        want_type <- isolate(wanted$type)
        selected <- keep
        if (!is.null(want_key) && identical(want_type, sel_type)) {
            selected <- want_key
            isolate(wanted$key <- NULL)
        }
        if (is.null(selected) || !selected %in% available$key) {
            selected <- if (nrow(available)) tail(available$key, 1) else NULL
        }
        updateVirtualSelect(
            "match_pick",
            choices = setNames(available$key, available$label),
            selected = selected
        )
    })
    
    selected_match <- reactive({
        row <- find_match(input$match_type, input$match_pick, matches_raw(), playoffs_raw())
        req(row, !is.na(row$red_score), !is.na(row$blue_score))
        row
    })
    
    output$match_heading <- renderUI({
        row <- selected_match()
        title <- if (identical(input$match_type, "P")) {
            as.character(row$match_label)
        } else {
            paste("Qualification", row$match_number)
        }
        winner <- if (row$red_score > row$blue_score) "red"
        else if (row$blue_score > row$red_score) "blue" else "tie"
        div(
            class = "match-heading",
            h2(title),
            div(
                class = "match-heading-score",
                rp_stack_ui(row, "red"),     # left of the red score
                span(class = paste("red-score", if (winner == "red") "win"), row$red_score),
                span(class = "dash", "\u2013"),
                span(class = paste("blue-score", if (winner == "blue") "win"), row$blue_score),
                rp_stack_ui(row, "blue")     # right of the blue score
            )
        )
    })
    
    output$match_lineup    <- renderUI(match_lineup(selected_match()))
    output$match_breakdown <- renderUI(match_detail_table(selected_match()))
    output$match_graphic   <- renderUI(match_graphic(selected_match()))
    
    # ---- API docs card (Scouting tab) ----
    # Shows the real address of this app so every example URL can be copied as-is
    output$api_docs <- renderUI({
        api_docs_ui(api_base_url(reactiveValuesToList(session$clientData)))
    })
    
    output$download_qual_matches <- downloadHandler(
        filename = function() { "quals.csv" },
        content = function(file) { write.csv(matches_data(), file, row.names = FALSE) }
    )
    
    output$download_playoffs <- downloadHandler(
        filename = function() { "playoffs.csv" },
        content = function(file) { 
            write.csv(
                playoffs_data() |> select(!ends_with(" RP")), 
                file, row.names = FALSE
            ) 
        }
    )
    
    output$download_rankings <- downloadHandler(
        filename = function() { "rankings.csv" },
        content = function(file) { write.csv(rankings_raw(), file, row.names = FALSE) }
    )
    
    output$download_teams <- downloadHandler(
        filename = function() { "teams.csv" },
        content = function(file) { write.csv(sort(unique(as.integer((unlist((c(matches_data()[, 2:7]))))))), file, row.names = FALSE) }
    )
    
    output$download_alliances <- downloadHandler(
        filename = function() { "alliances.csv" },
        content = function(file) { write.csv(alliances_raw(), file, row.names = FALSE) }
    )
}