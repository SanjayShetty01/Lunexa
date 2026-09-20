library(shiny)
library(bs4Dash)

box::use(
  ./modulus/mod_branding,
  ./modulus/mod_arbitrage_page,
  ./modulus/mod_sidebar_tab,
  ./modulus/mod_tab_main_page,
  ./modulus/mod_post_bet_hedge_page,
  ./modulus/mod_theme,
)

header <- bs4Dash::dashboardHeader(title = mod_branding$title)

sidebar <- bs4Dash::dashboardSidebar(
  mod_sidebar_tab$sidebarMenu(),
  minified = TRUE,
  skin = "dark"
)

footer <- bs4Dash::dashboardFooter(
  left  = "\u00a9 2026 Lunexa",
  right = "v1.0.0"
)

ui <- shiny::fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "numeric_input.css"),
    tags$link(rel = "stylesheet", href = "styles.css")
  ),

  bs4Dash::dashboardPage(
    header     = header,
    sidebar    = sidebar,
    body       = bs4DashBody(mod_tab_main_page$main_body("app")),
    footer     = footer,
    freshTheme = mod_theme$lunexa_theme(),
    dark       = NULL,
    help       = NULL
  )
)

server <- function(input, output, session) {
  mod_tab_main_page$main_body_server("app")
}


shiny::shinyApp(ui = ui, server = server)
