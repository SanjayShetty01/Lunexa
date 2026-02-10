box::use(./components/mod_numeric_input)
box::use(htmltools)
box::use(shinyWidgets)
box::use(shinyalert)
box::use(../functions/check_arbitrage)
box::use(../functions/utils)
box::use(../functions/stake_distribution_calculation)

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
    )),
    
    shiny::uiOutput(ns("arbitrage_result"))
    
  
  )
}

#'@export
arbitrage_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session){
    
    odd1  <- mod_numeric_input$numeric_input_server("arbitrage_odd1")
    odd2  <- mod_numeric_input$numeric_input_server("arbitrage_odd2")
    stake <- mod_numeric_input$numeric_input_server("total_stake")
    
    shiny::observeEvent(input$submit_value, {
      #browser()
      tryCatch({
        
        stopifnot(
          "Enter both the Odds and the Stake." = !is.na(odd1()) && 
            !is.na(odd2()) && !is.na(stake())
          )
        
        
        
        arbitrage_exist <- check_arbitrage$check_arbitrage(odd1 = odd1(), 
                                                           odd2 = odd2())
        implied_prob_sum <- utils$calculate_impl_prob_sum(odd1 = odd1(),
                                                      odd2 = odd2())
        
        stake_1 <- stake_distribution_calculation$calculate_stake(odd = odd1(),
                                                                  total_stake = stake(),
                                                                  impl_prob = implied_prob_sum)
        
        stake_2 <- stake_distribution_calculation$calculate_stake(odd = odd2(),
                                                                  total_stake = stake(),
                                                                  impl_prob = implied_prob_sum)
        
        if (isTRUE(arbitrage_exist)) {
          output$arbitrage_result <- shiny::renderUI({
            shiny::tagList(shiny::h2("Exists"),
                           shiny::h2(stake_1),
                           shiny::h2(stake_2))
          })
        } else {
          output$arbitrage_result <- shiny::renderUI({
            shiny::h2("No Exists")
          })
        }
      }, error = function(e) {
        shinyalert::shinyalert(title = "Error", type = "error",
                               text = e$message)
      })
      
    })
    
  })
  
}
  

