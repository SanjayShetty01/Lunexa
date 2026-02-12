box::use(./components/mod_numeric_input)
box::use(htmltools)
box::use(shinyWidgets)
box::use(shinyalert)
box::use(../functions/check_arbitrage)
box::use(../functions/utils)
box::use(../functions/stake_distribution_calculation)

#' UI for the pre-bet arbitrage calculator module
#'
#' Builds the input form and result area for the arbitrage calculation,
#' including odds, total stake inputs and the calculate button.
#'
#' @param id Character string used as the Shiny module namespace.
#'
#' @return A Shiny UI element (to be included inside a page layout).
#'
#' @export
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
    shiny::fluidRow(
      shiny::column(
        8,
        offset = 2,
        shinyWidgets::actionBttn(
          inputId = ns("submit_value"),
          label = "Calculate Arbitrage",
          style = "material-flat",
          color = "primary"
        ) |>
          htmltools::tagAppendAttributes(style = "width: inherit;")
      )
    ),
    
    shiny::br(),
    shiny::fluidRow(
      shiny::column(
        8,
        offset = 2,
        shiny::uiOutput(ns("arbitrage_result"))
      )
    )
    
  
  )
}

#' Server logic for the pre-bet arbitrage calculator module
#'
#' Validates inputs, checks whether an arbitrage opportunity exists,
#' computes the optimal stake distribution across the two odds and
#' renders a formatted summary in the UI.
#'
#' @param id Character string used as the Shiny module namespace.
#'
#' @return The server function is called for its side effects
#'   (rendering outputs and registering reactives) and does not
#'   return a value.
#'
#' @export
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
            shiny::wellPanel(
              shiny::h3(
                shiny::tags$span(
                  class = "label label-success",
                  "Arbitrage opportunity found"
                )
              ),
              shiny::tags$table(
                class = "table table-striped table-condensed",
                shiny::tags$tbody(
                  shiny::tags$tr(
                    shiny::tags$th("Stake on Odd 1"),
                    shiny::tags$td(
                      shiny::span(style = "color: #1a5276; font-weight: 600;", round(stake_1, 2))
                    )
                  ),
                  shiny::tags$tr(
                    shiny::tags$th("Stake on Odd 2"),
                    shiny::tags$td(
                      shiny::span(style = "color: #1a5276; font-weight: 600;", round(stake_2, 2))
                    )
                  ),
                  shiny::tags$tr(
                    shiny::tags$th("Sum of implied probabilities"),
                    shiny::tags$td(
                      shiny::span(style = "color: #1a5276; font-weight: 600;", round(implied_prob_sum, 2))
                    )
                  )
                )
              )
            )
          })
        } else {
          output$arbitrage_result <- shiny::renderUI({
            shiny::wellPanel(
              shiny::h3(
                shiny::tags$span(
                  class = "label label-danger",
                  "No arbitrage opportunity"
                )
              ),
              shiny::p(
                shiny::span(
                  style = "color: #c0392b;",
                  "With the current odds and stake, there is no arbitrage opportunity."
                )
              )
            )
          })
        }
      }, error = function(e) {
        shinyalert::shinyalert(title = "Error", type = "error",
                               text = e$message)
      })
      
    })
    
  })
  
}
  

