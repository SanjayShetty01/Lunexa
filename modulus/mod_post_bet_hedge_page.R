box::use(./components/mod_numeric_input)
box::use(htmltools)
box::use(shinyWidgets)
box::use(../functions/post_bet_arbitrage_calculation)

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
    exp_profit_amt <- mod_numeric_input$numeric_input_server("expected_profit_amt")
    exp_profit_pct_raw <- mod_numeric_input$numeric_input_server("expected_profit_pct")
    
    
    exp_profit_pct <- shiny::reactive({
      shiny::req(exp_profit_pct_raw())
      exp_profit_pct_raw() / 100
    })
    
    shiny::observeEvent(input$calculate_post_bet_arbitrage, {
      tryCatch({
        stopifnot(
          "Enter both the Odds and the Stake." = !is.na(wager()) && 
            !is.na(odd())
        )
        
        stopifnot("Enter Expected Amount/Percentage" = 
                    identical(input$calculation_type,'breakeven')
                    || !is.na(exp_profit_amt()) || 
                                !is.na(exp_profit_pct())
                  )
        
        if(identical(input$calculation_type, "breakeven")) {
          stake_2 <- post_bet_arbitrage_calculation$max_hedge_stake(
            stake_1 = wager(),
            odd_1 = odd()
          )
          
          required_odd <- post_bet_arbitrage_calculation$min_hedge_odds(
            stake_1 = wager(),
            stake_2 = stake_2
            )
        } else if(identical(input$calculation_type, "guaranteed_rupees")){
          max_required_stake_2 <- 
            post_bet_arbitrage_calculation$max_hedge_stake_for_profit(
            stake_1 = wager(),
            odd_1 = odd(),
            profit = exp_profit_amt())
          
          min_required_odd <- post_bet_arbitrage_calculation$min_hedge_odds_for_profit(
            stake_1 = wager(),
            stake_2 = max_required_stake_2,
            profit = exp_profit_amt()
          )
          
          required_odd <- post_bet_arbitrage_calculation$min_hedge_odds_absolute(
            stake_1 = wager(),
            odd_1 = odd(),
            profit = exp_profit_amt()
          )
          
          stake_2 <- post_bet_arbitrage_calculation$min_hedge_stake_for_profit(
            stake_1 = wager(),
            odd_2 = required_odd,
            profit = exp_profit_amt()
          )
        } else if(identical(input$calculation_type, "guaranteed_percent")) {
          
          required_odd <- post_bet_arbitrage_calculation$max_hedge_stake_pct(
            stake_1 = wager(),
            odd_1 = odd(),
            p = exp_profit_pct()
          )
          
          stake_2 <- post_bet_arbitrage_calculation$min_hedge_odds_pct_absolute(
            odd_1 = odd(),
            p = exp_profit_pct()
          )
          
        } else {
          stop("Unexpected Error")
        }
        
        output$post_bet_arbitrage <- shiny::renderUI({
          shiny::tagList(
            shiny::h2(required_odd),
            shiny::h2(stake_2)
          )
          
        })
        
      }, error = function(e) {
        shinyalert::shinyalert(title = "Error", type = "error",
                               text = e$message)
      }
      )
    })
    
  })
  
}


