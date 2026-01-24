box::use(./components/mod_numeric_input)
box::use(htmltools)
box::use(shinyWidgets)

#'@export
post_bet_hedge_page_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    
    shiny::br(),
    shiny::fluidRow(shiny::column(
      6,
      mod_numeric_input$numeric_input_ui(
        id = ns("entered_wage"),
        label = "Entered Wager",
        step = 0.01,
      )
    ),
    
    shiny::br(),
    shiny::column(
      6,
      mod_numeric_input$numeric_input_ui(
        id = ns("entered_odd"),
        label = "Entered Odd",
        step = 0.01,
      )
    )
    ),
    shiny::br(),
    shiny::fluidRow(shiny::column(
      6,
      offset = 3
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
post_bet_hedge_page_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session){
    
    odd1  <- mod_numeric_input$numeric_input_server("arbitrage_odd1")
    odd2  <- mod_numeric_input$numeric_input_server("arbitrage_odd2")
    stake <- mod_numeric_input$numeric_input_server("total_stake")
  })
  
}


