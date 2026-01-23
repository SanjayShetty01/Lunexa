box::use(./components/mod_numeric_input)

#'@export
aarbitrage_UI <- function(id){
  ns <- shiny::NS(id)
  
  shiny::fluidPage(
    shiny::fluidRow(
      mod_numeric_input$numeric_input_ui(id = ns("arbitrage_odd1"),
                                         label = "Enter the Odd 1",
                                         value = NA,
                                         width = 6,
                                         step = 0.01)
    ),
    
    shiny::fluidRow(
      mod_numeric_input$numeric_input_ui(id = ns("arbitrage_odd2"),
                                         label = "Enter the Odd 2",
                                         value = NA,
                                         width = 6,
                                         step = 0.01)
    ),
    
    shiny::fluidRow(
      mod_numeric_input$numeric_input_ui(id = ns("total_stake"),
                                         label = "Total Stake",
                                         value = NA,
                                         width = 6,
                                         step = 1)
    )

  )
}

#'@export
arbitrage_server <- function(id) {
  shiny::moduleServer(id, function(input, output, server){
    mod_numeric_input$numeric_input_server(id)
  })
  
}
  

