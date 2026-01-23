box::use(./components/mod_numeric_input)

#'@export
arbitrage_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(shiny::column(
      6,
      mod_numeric_input$numeric_input_ui(
        id = ns("arbitrage_odd1"),
        label = "Enter the Odd 1",
        step = 0.01,
      )
    )),
    shiny::fluidRow(shiny::column(
      6,
      mod_numeric_input$numeric_input_ui(
        id = ns("arbitrage_odd2"),
        label = "Enter the Odd 2",
        step = 0.01,
      )
    )),
    shiny::fluidRow(shiny::column(
      6,
      mod_numeric_input$numeric_input_ui(
        id = ns("total_stake"),
        label = "Total Stake",
        step = 1,
      )
    )),
    
    
    # shiny::fluidRow(shiny::column(
    #   6,
    #   shiny::actionButton(
    #     id = ("total_stake"),
    #     label = "Total Stake",
    #     step = 1,
    #   )
    # ))
    
  
  )
}

#'@export
arbitrage_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session){
    
    odd1  <- mod_numeric_input$numeric_input_server("arbitrage_odd1")
    odd2  <- mod_numeric_input$numeric_input_server("arbitrage_odd2")
    stake <- mod_numeric_input$numeric_input_server("total_stake")
  })
  
}
  

