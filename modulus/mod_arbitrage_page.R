box::use(./components/mod_numeric_input)
box::use(htmltools)
box::use(shinyWidgets)

#'@export
arbitrage_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    
    shiny::br(),
    shiny::fluidRow(shiny::column(
      6,
      mod_numeric_input$numeric_input_ui(
        id = ns("arbitrage_odd1"),
        label = "Enter the Odd 1",
        step = 0.01,
      )
    ),
    
    shiny::br(),
    shiny::column(
      6,
      mod_numeric_input$numeric_input_ui(
        id = ns("arbitrage_odd2"),
        label = "Enter the Odd 2",
        step = 0.01,
      )
    )
    ),
    shiny::br(),
    shiny::fluidRow(shiny::column(
      6,
      offset = 3,
      mod_numeric_input$numeric_input_ui(
        id = ns("total_stake"),
        label = "Total Stake",
        step = 1,
      )
    )),
    
    shiny::br(),
    shiny::fluidRow(shiny::column(
      8,
      offset = 2,
      shinyWidgets::actionBttn(
        inputId = ns("submit_value"),
        label = "Calculate Arbitrage",
        style = "material-flat",
        color = "primary"
      ) |>
        htmltools::tagAppendAttributes(style = "width: inherit;")
    ))
    
  
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
  

