library(htmltools)
library(dplyr)

# =============================================================================
# Match Details: scoring rules, match catalog, breakdown table, field graphic
# =============================================================================

# ---- Scoring rules ----------------------------------------------------------
# Edit these if the game manual changes. Auto / teleop / endgame are not split.
SCORING <- list(
    taxi        = 2,                                      # per robot
    park        = 2,                                      # per robot
    carrot      = c(L1 = 3, L2 = 4, L3 = 5, oven = 2),    # per carrot
    cake        = c(L1 = 8, L2 = 10, L3 = 12, oven = 1),  # per cake
    carrot_haul = 1,                                      # per hauled carrot
    cake_haul   = 3,                                      # per hauled cake
    minor_foul  = 4,                                      # awarded to the OTHER alliance
    major_foul  = 8                                       # awarded to the OTHER alliance
)
# Hauled items only score if that robot is also parked.

# Which alliance is drawn on the left of the field graphic
# ("red" matches the tables and lineup, which are red | blue).
GRAPHIC_LEFT <- "red"
PANTRY_SLOTS <- 5L  # slots per shelf

# ---- Match catalog ----------------------------------------------------------
# One row per match across both lists. `key` is what the selector stores
# (qualification number, or the playoff label), `played` is TRUE once both
# scores exist.
match_catalog <- function(quals, playoffs) {
    one <- function(df, type, key, label) {
        if (nrow(df) == 0) {
            return(tibble(type = character(), key = character(),
                          label = character(), played = logical()))
        }
        tibble(
            type = type, key = key, label = label,
            played = !is.na(df$red_score) & !is.na(df$blue_score)
        )
    }
    bind_rows(
        one(quals, "Q", as.character(quals$match_number),
            paste("Qual", quals$match_number)),
        one(playoffs, "P", as.character(playoffs$match_label),
            as.character(playoffs$match_label))
    )
}

# The raw row for a catalog key, or NULL
find_match <- function(type, key, quals, playoffs) {
    if (is.null(type) || is.null(key) || !nzchar(key)) return(NULL)
    hit <- if (identical(type, "P")) {
        playoffs[as.character(playoffs$match_label) == key, ]
    } else {
        quals[as.character(quals$match_number) == key, ]
    }
    if (nrow(hit) == 0) NULL else hit[1, ]
}


# ---- Per-alliance numbers ---------------------------------------------------
# `row` is one row of the raw match tibble (flatten_match() columns).
alliance_breakdown <- function(row, side) {
    s   <- if (side == "red") "R" else "B"
    opp <- if (side == "red") "blue" else "red"
    
    get  <- function(name) if (name %in% names(row)) row[[name]][1] else NA
    num  <- function(name) { v <- suppressWarnings(as.numeric(get(name))); if (is.na(v)) 0 else v }
    flag <- function(name) isTRUE(as.logical(get(name)))
    
    robots <- 1:3
    taxi <- vapply(robots, function(i) flag(paste0("taxi_", s, i)), logical(1))
    park <- vapply(robots, function(i) flag(paste0("park_", s, i)), logical(1))
    haul_carrots <- vapply(robots, function(i) num(paste0("carrot_haul_", s, i)), numeric(1))
    haul_cakes   <- vapply(robots, function(i) num(paste0("cake_haul_", s, i)), numeric(1))
    
    levels <- c("L1", "L2", "L3")
    carrots <- vapply(levels, function(l) num(paste0("carrot_", l, "_", side)), numeric(1))
    cakes   <- vapply(levels, function(l) num(paste0("cake_", l, "_", side)), numeric(1))
    carrot_oven <- num(paste0("carrot_oven_", side))
    cake_oven   <- num(paste0("cake_oven_", side))
    
    # Hauls only count for a robot that is also parked
    haul_pts <- ifelse(park, haul_carrots * SCORING$carrot_haul + haul_cakes * SCORING$cake_haul, 0)
    
    minor <- num(paste0("minor_", side)); major <- num(paste0("major_", side))
    foul_pts <- num(paste0("minor_", opp)) * SCORING$minor_foul +
        num(paste0("major_", opp)) * SCORING$major_foul
    
    list(
        side = side, s = s,
        teams = vapply(robots, function(i) {
            t <- get(paste0(side, i)); if (is.na(t)) "–" else as.character(t)
        }, character(1)),
        taxi = taxi, taxi_pts = sum(taxi) * SCORING$taxi,
        carrots = carrots, carrot_pts = sum(carrots * SCORING$carrot[levels]),
        cakes = cakes, cake_pts = sum(cakes * SCORING$cake[levels]),
        carrot_oven = carrot_oven, cake_oven = cake_oven,
        oven_pts = carrot_oven * SCORING$carrot["oven"] + cake_oven * SCORING$cake["oven"],
        park = park, park_pts = sum(park) * SCORING$park,
        haul_carrots = haul_carrots, haul_cakes = haul_cakes, haul_pts = haul_pts,
        minor = minor, major = major, foul_pts = foul_pts,
        stocked = flag(paste0("stocked_rp_", side)),
        baked   = flag(paste0("baked_rp_", side)),
        dinner  = flag(paste0("dinner_rp_", side)),
        score   = suppressWarnings(as.numeric(get(paste0(side, "_score"))))
    )
}


# ---- Top-down numerical table (TBA style) -----------------------------------
match_detail_table <- function(row) {
    r <- alliance_breakdown(row, "red")
    b <- alliance_breakdown(row, "blue")
    
    pts   <- function(x) paste0("(+", x, ")")
    tick  <- function(x) ifelse(x, "✓", "✗")
    plural <- function(n, word) paste0(n, " ", word, if (n == 1) "" else "s")
    
    # A row with one value per alliance, spanning the two side columns
    single <- function(label, rv, bv, class = NULL) {
        tags$tr(
            class = class,
            tags$td(class = "red-cell", colspan = 2, rv),
            tags$td(class = "label-cell", label),
            tags$td(class = "blue-cell", colspan = 2, bv)
        )
    }
    # A block of sub-rows (L3 / L2 / L1 ...) sharing one middle label
    block <- function(label, names, rvals, bvals) {
        lapply(seq_along(names), function(i) {
            tags$tr(
                tags$td(class = "red-cell sub", names[i]),
                tags$td(class = "red-cell", rvals[i]),
                if (i == 1) tags$td(class = "label-cell", rowspan = length(names), label),
                tags$td(class = "blue-cell", bvals[i]),
                tags$td(class = "blue-cell sub", names[i])
            )
        })
    }
    
    taxi_txt <- function(x) paste(paste(tick(x$taxi), collapse = " "), pts(x$taxi_pts))
    haul_txt <- function(x, i) {
        c <- x$haul_carrots[i]; k <- x$haul_cakes[i]
        if (c + k == 0) return("–")
        items <- paste(plural(c, "carrot"), plural(k, "cake"), sep = ", ")
        if (x$park[i]) paste(items, pts(x$haul_pts[i]))
        else paste(items, "(+0, not parked)")
    }
    park_txt <- function(x, i) {
        paste0(x$teams[i], " · ", if (x$park[i]) paste("Parked", pts(SCORING$park)) else "Not parked")
    }
    
    rows <- c(
        list(single("Taxi", taxi_txt(r), taxi_txt(b))),
        block("Pantry Carrots", c("L3", "L2", "L1"), rev(r$carrots), rev(b$carrots)),
        list(single("Pantry Carrot Points", r$carrot_pts, b$carrot_pts, "subtotal")),
        block("Pantry Cakes", c("L3", "L2", "L1"), rev(r$cakes), rev(b$cakes)),
        list(single("Pantry Cake Points", r$cake_pts, b$cake_pts, "subtotal")),
        block("Oven", c("Carrots", "Cakes"),
              c(r$carrot_oven, r$cake_oven), c(b$carrot_oven, b$cake_oven)),
        list(single("Oven Points", r$oven_pts, b$oven_pts, "subtotal")),
        lapply(1:3, function(i) single(paste("Robot", i, "Park"), park_txt(r, i), park_txt(b, i))),
        lapply(1:3, function(i) single(paste("Robot", i, "Haul"), haul_txt(r, i), haul_txt(b, i))),
        list(single("Endgame Points",
                    r$park_pts + sum(r$haul_pts), b$park_pts + sum(b$haul_pts), "subtotal")),
        list(single("Fouls Committed",
                    paste0(r$minor, " minor · ", r$major, " major"),
                    paste0(b$minor, " minor · ", b$major, " major"))),
        list(single("Foul Points Earned", r$foul_pts, b$foul_pts, "subtotal")),
        list(single("Stocked RP", tick(r$stocked), tick(b$stocked))),
        list(single("Baked RP",   tick(r$baked),   tick(b$baked))),
        list(single("Dinner RP",  tick(r$dinner),  tick(b$dinner))),
        list(single("Final Score",
                    ifelse(is.na(r$score), "–", r$score),
                    ifelse(is.na(b$score), "–", b$score),
                    "total"))
    )
    
    tags$table(
        class = "tba-table detail-table",
        tags$thead(tags$tr(
            tags$th(colspan = 2, class = "grp-red", "Red Alliance"),
            tags$th(""),
            tags$th(colspan = 2, class = "grp-blue", "Blue Alliance")
        )),
        tags$tbody(rows)
    )
}


# ---- Field graphic (inline SVG) ---------------------------------------------
# Colours live in www/styles.css (.mg-* classes) so the art can be restyled
# without touching R.

# "1,0,2,1" -> c(1, 0, 2, 1). NA / "" -> NULL
parse_slots <- function(x) {
    if (is.null(x) || length(x) == 0 || is.na(x[1]) || !nzchar(x[1])) return(NULL)
    v <- suppressWarnings(as.integer(strsplit(x[1], ",", fixed = TRUE)[[1]]))
    v[is.na(v)] <- 0L
    v
}

# Slot contents for one pantry shelf (0 empty, 1 carrot, 2 cake). Uses the
# arena's slot positions when present, otherwise fills from the left with
# carrots first, then cakes.
shelf_slots <- function(row, shelf, side) {
    col <- paste0("slots_L", shelf, "_", side)
    v <- if (col %in% names(row)) parse_slots(row[[col]]) else NULL
    if (is.null(v)) {
        n_carrot <- suppressWarnings(as.integer(row[[paste0("carrot_L", shelf, "_", side)]]))
        n_cake   <- suppressWarnings(as.integer(row[[paste0("cake_L", shelf, "_", side)]]))
        v <- c(rep(1L, max(0L, n_carrot, na.rm = TRUE)), rep(2L, max(0L, n_cake, na.rm = TRUE)))
    }
    v[!v %in% c(1L, 2L)] <- 0L
    c(v, rep(0L, max(0L, PANTRY_SLOTS - length(v))))[seq_len(PANTRY_SLOTS)]
}

# Items carried by one robot (1 carrot, 2 cake), in order
haul_items <- function(row, side, i) {
    s <- if (side == "red") "R" else "B"
    col <- paste0("slots_haul_", s, i)
    v <- if (col %in% names(row)) parse_slots(row[[col]]) else NULL
    if (is.null(v)) {
        n_carrot <- suppressWarnings(as.integer(row[[paste0("carrot_haul_", s, i)]]))
        n_cake   <- suppressWarnings(as.integer(row[[paste0("cake_haul_", s, i)]]))
        v <- c(rep(1L, max(0L, n_carrot, na.rm = TRUE)), rep(2L, max(0L, n_cake, na.rm = TRUE)))
    }
    v[v %in% c(1L, 2L)]
}

# One carrot / cake / empty bar
svg_item <- function(x, y, w, h, kind) {
    cls <- c(`0` = "mg-empty", `1` = "mg-carrot", `2` = "mg-carrot")[[as.character(kind)]]
    bar <- sprintf('<rect class="%s" x="%.1f" y="%.1f" width="%.1f" height="%.1f" rx="%.1f"/>',
                   cls, x, y, w, h, h * 0.35)
    if (kind == 2) {
        sw <- w * 0.14
        bar <- paste0(bar, sprintf(
            '<rect class="mg-stripe" x="%.1f" y="%.1f" width="%.1f" height="%.1f"/>',
            x + w / 2 - sw / 2, y, sw, h))
    }
    bar
}

svg_text <- function(x, y, label, class, anchor = "middle") {
    sprintf('<text class="%s" x="%.1f" y="%.1f" text-anchor="%s">%s</text>',
            class, x, y, anchor, htmlEscape(label))
}

svg_pantry <- function(row, side, x0, y0, width = 425) {
    pitch <- 70; bar_w <- 66; bar_h <- 24; gap <- 14; post <- 18
    inner <- width - 2 * post
    start <- x0 + post + (inner - (PANTRY_SLOTS * bar_w + (PANTRY_SLOTS - 1) * gap)) / 2
    
    out <- sprintf('<rect class="mg-%s-soft" x="%d" y="%d" width="%d" height="%d"/>',
                   side, x0, y0, width, 3 * pitch)
    for (k in 0:2) {                                   # top shelf is L3
        shelf <- 3 - k
        slots <- shelf_slots(row, shelf, side)
        for (j in seq_len(PANTRY_SLOTS)) {
            out <- paste0(out, svg_item(start + (j - 1) * (bar_w + gap),
                                        y0 + k * pitch + 22, bar_w, bar_h, slots[j]))
        }
        out <- paste0(out, sprintf('<rect class="mg-%s" x="%d" y="%d" width="%d" height="14"/>',
                                   side, x0, y0 + k * pitch + 56, width))
    }
    paste0(out,
           sprintf('<rect class="mg-%s" x="%d" y="%d" width="%d" height="%d"/>', side, x0, y0, post, 3 * pitch),
           sprintf('<rect class="mg-%s" x="%d" y="%d" width="%d" height="%d"/>', side, x0 + width - post, y0, post, 3 * pitch))
}

svg_oven <- function(row, side, x0, y0) {
    w <- 345; h <- 170
    cakes   <- suppressWarnings(as.integer(row[[paste0("cake_oven_", side)]]))
    carrots <- suppressWarnings(as.integer(row[[paste0("carrot_oven_", side)]]))
    cakes   <- if (is.na(cakes)) 0L else cakes
    carrots <- if (is.na(carrots)) 0L else carrots
    paste0(
        sprintf('<rect class="mg-%s-soft mg-oven" x="%d" y="%d" width="%d" height="%d" rx="36"/>',
                side, x0, y0, w, h),
        svg_item(x0 + 40, y0 + 22, 150, 48, 2), svg_text(x0 + w - 85, y0 + 60, cakes, "mg-count"),
        svg_item(x0 + 40, y0 + 100, 150, 48, 1), svg_text(x0 + w - 85, y0 + 138, carrots, "mg-count")
    )
}

svg_robot <- function(row, side, i, team, parked, cx, cy, angle) {
    half <- 48
    items <- haul_items(row, side, i)
    shown <- head(items, 8)
    out <- sprintf('<g transform="translate(%.1f %.1f) rotate(%d)">', cx, cy, angle)
    out <- paste0(out, sprintf(
        '<rect class="%s" x="%d" y="%d" width="%d" height="%d" rx="4"><title>%s</title></rect>',
        if (parked) paste0("mg-", side) else "mg-off", -half, -half, 2 * half, 2 * half,
        htmlEscape(paste0("Team ", team, if (parked) " (parked)" else " (not parked)"))))
    out <- paste0(out, svg_text(0, -30, paste0("R", i), if (parked) "mg-robot-no" else "mg-robot-no mg-team-off"))
    out <- paste0(out, svg_text(0, -8, team, if (parked) "mg-team" else "mg-team mg-team-off"))
    for (n in seq_along(shown)) {
        col <- (n - 1) %% 2; rw <- (n - 1) %/% 2
        out <- paste0(out, svg_item(-37 + col * 40, 0 + rw * 12, 34, 9, shown[n]))
    }
    if (length(items) > length(shown)) {
        out <- paste0(out, svg_text(0, 44, paste0("+", length(items) - length(shown)), "mg-more"))
    }
    paste0(out, "</g>")
}

svg_endgame <- function(row, side, cx, ty) {
    teams <- vapply(1:3, function(i) {
        t <- row[[paste0(side, i)]]; if (is.na(t)) "–" else as.character(t)
    }, character(1))
    s <- if (side == "red") "R" else "B"
    parked <- vapply(1:3, function(i) isTRUE(as.logical(row[[paste0("park_", s, i)]])), logical(1))
    
    paste0(
        sprintf('<polygon class="mg-%s-soft mg-tri" points="%.1f,%d %.1f,%d %.1f,%d"/>',
                side, cx - 142, ty, cx + 142, ty, cx, ty + 140),
        svg_robot(row, side, 1, teams[1], parked[1], cx - 112, ty + 150, 40),
        svg_robot(row, side, 2, teams[2], parked[2], cx,       ty + 200, 0),
        svg_robot(row, side, 3, teams[3], parked[3], cx + 112, ty + 150, -40)
    )
}

match_graphic <- function(row) {
    right <- if (GRAPHIC_LEFT == "blue") "red" else "blue"
    sides <- c(GRAPHIC_LEFT, right)
    x0    <- c(10, 449)                 # pantry panels
    ox    <- c(65, 500)                 # ovens
    cx    <- c(222, 661)                # endgame centres
    
    score <- function(side) {
        v <- row[[paste0(side, "_score")]]; if (is.na(v)) "" else paste0(" · ", v)
    }
    out <- ""
    for (k in 1:2) {
        side <- sides[k]
        out <- paste0(
            out,
            svg_text(x0[k] + 212, 26, paste0(tools::toTitleCase(side), score(side)),
                     paste0("mg-caption mg-caption-", side)),
            svg_pantry(row, side, x0[k], 68),
            svg_oven(row, side, ox[k], 326),
            svg_endgame(row, side, cx[k], 552)
        )
    }
    labels <- paste0(
        svg_text(10, 58, "PANTRY", "mg-section", "start"),
        svg_text(10, 316, "OVEN", "mg-section", "start"),
        svg_text(10, 542, "ENDGAME", "mg-section", "start")
    )
    
    HTML(paste0(
        '<svg class="match-graphic" viewBox="0 0 884 810" role="img" ',
        'aria-label="Field graphic: pantry shelves, oven counts and endgame robots for each alliance" ',
        'xmlns="http://www.w3.org/2000/svg">', labels, out, '</svg>',
        '<div class="mg-legend">',
        '<span><svg viewBox="0 0 30 12" width="30" height="12"><rect class="mg-carrot" width="30" height="12" rx="4"/></svg> Carrot</span>',
        '<span><svg viewBox="0 0 30 12" width="30" height="12"><rect class="mg-carrot" width="30" height="12" rx="4"/><rect class="mg-stripe" x="12.5" width="5" height="12"/></svg> Cake</span>',
        '<span><svg viewBox="0 0 30 12" width="30" height="12"><rect class="mg-empty" width="30" height="12" rx="4"/></svg> Empty slot</span>',
        '<span><svg viewBox="0 0 12 12" width="12" height="12"><rect class="mg-off" width="12" height="12" rx="2"/></svg> Not parked</span>',
        '</div>'
    ))
}


# ---- Lineup strip -----------------------------------------------------------
# Robot 1 / 2 / 3 for each alliance, in the same order the Score Breakdown and
# the field graphic use. Team numbers are clickable (see "team-cell" in script.js).
match_lineup <- function(row) {
    one <- function(side) {
        s <- if (side == "red") "R" else "B"
        chips <- lapply(1:3, function(i) {
            team <- row[[paste0(side, i)]]
            has_team <- !is.na(team)
            parked <- isTRUE(as.logical(row[[paste0("park_", s, i)]]))
            div(
                class = "lineup-chip",
                span(class = "lineup-robot", paste0("Robot ", i)),
                span(class = paste("lineup-team", if (has_team) "team-cell"),
                     if (has_team) as.character(team) else "\u2013")
            )
        })
        div(class = paste("lineup-side", paste0("lineup-", side)),
            div(class = "lineup-chips", chips))
    }
    div(class = "match-lineup", one("red"), one("blue"))
}


# ---- RP badges beside the heading score -------------------------------------
# Small vertical S / B / D stack, filled when the bonus RP was earned. Returns
# NULL when the flags aren't known, same as the results tables.
rp_stack_ui <- function(row, side) {
    flags <- c(
        S = row[[paste0("stocked_rp_", side)]],
        B = row[[paste0("baked_rp_", side)]],
        D = row[[paste0("dinner_rp_", side)]]
    )
    if (anyNA(flags)) return(NULL)
    names <- c(S = "Stocked", B = "Baked", D = "Dinner")
    div(
        class = paste("rp-stack", paste0("rp-", side)),
        lapply(names(flags), function(letter) {
            earned <- isTRUE(flags[[letter]])
            span(
                class = paste("rp", if (earned) "on" else "off"),
                title = paste0(names[[letter]], " RP ", if (earned) "earned" else "not earned"),
                letter
            )
        })
    )
}