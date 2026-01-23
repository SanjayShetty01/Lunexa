#' @export
sidebarMenu <- function(){
  bs4Dash::sidebarMenu(
    bs4Dash::menuItem(
      text = "Introduction",
      tabName = "introduction",
      icon = shiny::icon("eye")
    ),
    bs4Dash::menuItem(
      text = "Arbitrage",
      tabName = "arbitrage_tab",
      icon = shiny::icon("table")
    ),
    bs4Dash::menuItem(
      text = "Post Bet Hedge",
      tabName = "post_bet_hedge",
      icon = shiny::icon("chart-simple")
    )
  )
}