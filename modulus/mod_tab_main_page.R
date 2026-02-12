box::use(./mod_arbitrage_page)
box::use(./mod_post_bet_hedge_page)


#' Main body content for the application
#'
#' Builds the set of tab items shown in the dashboard body, including
#' the introduction page, arbitrage calculator, and post-bet hedge
#' calculator.
#'
#' @param id Character string used as the Shiny module namespace.
#'
#' @return A \code{bs4Dash::tabItems} UI object.
#'
#' @export
main_body <- function(id){
  
  ns <- shiny::NS(id)
  
  bs4Dash::tabItems(
    bs4Dash::tabItem(
      tabName = 'introduction',
      shiny::includeHTML("www/introduction.html")
    ),
    bs4Dash::tabItem(
      tabName = 'arbitrage_tab',
      mod_arbitrage_page$arbitrage_UI(ns("arbitrage_tab"))
    ),
    bs4Dash::tabItem(
      tabName = 'post_bet_hedge',
      mod_post_bet_hedge_page$post_bet_hedge_page_UI(ns("bet_hedge_tab"))
    )
  )
}

#' Server logic for the main body module
#'
#' Registers the server components for the arbitrage and post-bet hedge
#' modules inside the main dashboard body.
#'
#' @param id Character string used as the Shiny module namespace.
#'
#' @return The server function is called for its side effects and does
#'   not return a value.
#'
#' @export
main_body_server <- function(id){
  
  shiny::moduleServer(id ,function(id,input, output, session){
    mod_arbitrage_page$arbitrage_server("arbitrage_tab")
    mod_post_bet_hedge_page$post_bet_hedge_page_server("bet_hedge_tab")
  })
}


