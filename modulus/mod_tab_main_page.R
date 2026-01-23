box::use(./mod_arbitrage_page)

#'@export
main_body <- function(id){
  
  ns <- shiny::NS(id)
  
  bs4Dash::tabItems(
    bs4Dash::tabItem(
      tabName = 'introduction',
      shiny::includeHTML("www/introduction.html")
    ),
    bs4Dash::tabItem(
      tabName = 'arbitrage_tab',
      mod_arbitrage_page$aarbitrage_UI("arbitrage_tab")
    ),
    bs4Dash::tabItem(
      tabName = 'post_bet_hedge'
    )
  )
}

#'@export
main_body_server <- function(id){
  
  ns <- shiny::NS(id)
  
  shiny::moduleServer(id, function(input, output, session){
    mod_arbitrage_page$arbitrage_server(ns("arbitrage_tab"))
    
  })
}


