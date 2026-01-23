library(shiny)
library(bs4Dash)

box::use(
  ./modulus/mod_arbitrage_page,
  ./modulus/mod_sidebar_tab,
  ./modulus/mod_tab_main_page
)

header <- bs4Dash::dashboardHeader(title = "Lunexa")

sidebar <- bs4Dash::dashboardSidebar(mod_sidebar_tab$sidebarMenu(), 
                                     minified = T)

body <- bs4DashBody(mod_tab_main_page$main_body('app'))

ui <- shiny::fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", href = "numeric_input.css")
    ),
  
  bs4Dash::dashboardPage(
    header = header,
    sidebar = sidebar,
    body = body,
  )
)

server <- function(input, output, session) {
  mod_tab_main_page$main_body_server('app')
}


shiny::shinyApp(ui = ui, server = server)
