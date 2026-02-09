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
      offset = 3,
      shiny::selectInput(
        inputId = ns("calculation_type"),
        label = "Profit or Breakeven",
        choices = list("Breakeven" = "breakeven", 
                       "Guaranteed Profit in Rupees" = "guaranteed_rupees", 
                       "Guaranteed Profit in %" = "guaranteed_percent"),
        selected = 'breakeven'
      )
    )),
    
    shiny::br(),
    
    shiny::conditionalPanel(
      condition = "input.calculation_type === 'breakeven'", 
      ns = ns
    ),
    shiny::conditionalPanel(
      condition = "input.calculation_type === 'guaranteed_rupees'",
      mod_numeric_input$numeric_input_ui(
        id = ns("expected_profit_amt"),
        label = "Expected Profit (Amount)",
        step = 1,
      ),
      ns = ns
      ),
    
    shiny::conditionalPanel(
      condition = "input.calculation_type === 'guaranteed_percent'", 
      mod_numeric_input$numeric_input_ui(
        id = ns("expected_profit_pct"),
        label = "Expected Profit (Percentage)",
        step = 1,
      ),
      ns = ns
    ),
    
    shiny::br(),
    shiny::fluidRow(shiny::column(
      8,
      offset = 2,
      shinyWidgets::actionBttn(
        inputId = ns("calculate_post_bet_arbitrage"),
        label = "Calculate Arbitrage",
        style = "material-flat",
        color = "primary"
      ) |>
        htmltools::tagAppendAttributes(style = "width: inherit;")
    )),
    
    shiny::uiOutput(ns("post_bet_arbitrage"))
  )
}

#'@export
post_bet_hedge_page_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session){
    
    wager  <- mod_numeric_input$numeric_input_server("entered_wage")
    odd  <- mod_numeric_input$numeric_input_server("entered_odd")
    exp_profit <- mod_numeric_input$numeric_input_server("expected_profit_amt")
    
    shiny::observeEvent(input$calculate_post_bet_arbitrage, {
      tryCatch({
        stopifnot(
          "Enter both the Odds and the Stake." = !is.na(wager()) && 
            !is.na(odd()) && !is.na(exp_profit())
        )
        
        output$post_bet_arbitrage <- shiny::renderUI({
          shiny::h2("LOL")
        })
        
      }, error = function(e) {
        shinyalert::shinyalert(title = "Error", type = "error",
                               text = e$message)
      }
      )
    })
    
  })
  
}


