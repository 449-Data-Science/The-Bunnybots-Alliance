# Entry point. The page and server logic live in app-ui.R and app-server.R
# (the old ui.R / server.R). They have to be named something else because Shiny
# ignores app.R whenever a server.R exists, and app.R is what lets the public
# API in R/api.R answer requests like /api/matches/qualification ahead of the page.
library(shiny)

source("global.R")
page   <- source("app-ui.R",     local = TRUE)$value
server <- source("app-server.R", local = TRUE)$value

ui <- function(req) {
    if (is_api_request(req)) return(api_response(req))
    page
}

# ".*" lets non-root paths reach `ui`; static files in www/ are served first
shinyApp(ui, server, uiPattern = ".*")
