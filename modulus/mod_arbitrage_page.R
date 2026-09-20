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

    # ── Odds Input Card ───────────────────────────────────
    shiny::fluidRow(
      shiny::column(
        12,
        bs4Dash::box(
          title = shiny::tagList(shiny::icon("calculator"), " Enter Odds"),
          status = "primary",
          solidHeader = FALSE,
          collapsible = FALSE,
          width = 12,
          shiny::fluidRow(
            shiny::column(
              6,
              mod_numeric_input$numeric_input_ui(
                id = ns("arbitrage_odd1"),
                label = "Odd 1",
                step = 0.01
              )
            ),
            shiny::column(
              6,
              mod_numeric_input$numeric_input_ui(
                id = ns("arbitrage_odd2"),
                label = "Odd 2",
                step = 0.01
              )
            )
          )
        )
      )
    ),

    # ── Stake Input Card ──────────────────────────────────
    shiny::fluidRow(
      shiny::column(
        8, offset = 2,
        bs4Dash::box(
          title = shiny::tagList(shiny::icon("coins"), " Total Stake"),
          status = "warning",
          solidHeader = FALSE,
          collapsible = FALSE,
          width = 12,
          mod_numeric_input$numeric_input_ui(
            id = ns("total_stake"),
            label = "Amount",
            step = 1
          )
        )
      )
    ),

    # ── Calculate Button ──────────────────────────────────
    shiny::br(),
    shiny::fluidRow(
      shiny::column(
        8, offset = 2,
        shinyWidgets::actionBttn(
          inputId = ns("submit_value"),
          label = "Calculate Arbitrage",
          style = "material-flat",
          color = "primary"
        ) |>
          htmltools::tagAppendAttributes(style = "width: inherit;")
      )
    ),

    # ── Results ───────────────────────────────────────────
    shiny::br(),
    shiny::fluidRow(
      shiny::column(
        10, offset = 1,
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
      tryCatch({

        stopifnot(
          "Enter both the Odds and the Stake." = !is.na(odd1()) &&
            !is.na(odd2()) && !is.na(stake())
          )

        arbitrage_exist <- check_arbitrage$check_arbitrage(
          odd1 = odd1(),
          odd2 = odd2()
        )
        implied_prob_sum <- utils$calculate_impl_prob_sum(
          odd1 = odd1(),
          odd2 = odd2()
        )

        stake_1 <- stake_distribution_calculation$calculate_stake(
          odd = odd1(),
          total_stake = stake(),
          impl_prob = implied_prob_sum
        )

        stake_2 <- stake_distribution_calculation$calculate_stake(
          odd = odd2(),
          total_stake = stake(),
          impl_prob = implied_prob_sum
        )

        stake_1_class <- if (stake_1 < 0) "text-loss" else "text-profit"
        stake_2_class <- if (stake_2 < 0) "text-loss" else "text-profit"
        implied_prob_class <- if (implied_prob_sum >= 1) "text-loss" else "text-profit"

        if (isTRUE(arbitrage_exist)) {
          output$arbitrage_result <- shiny::renderUI({
            shiny::tagList(
              # Status banner
              shiny::div(
                class = "result-card result-card-success",
                shiny::h3(
                  shiny::tags$span(
                    class = "label label-success-custom",
                    shiny::icon("check-circle"),
                    " Arbitrage opportunity found"
                  )
                )
              ),

              shiny::br(),

              # Value boxes
              shiny::fluidRow(
                shiny::column(
                  4,
                  bs4Dash::valueBox(
                    value = round(stake_1, 2),
                    subtitle = "Stake on Odd 1",
                    icon = shiny::icon("coins"),
                    color = if (stake_1 < 0) "danger" else "success",
                    width = 12
                  )
                ),
                shiny::column(
                  4,
                  bs4Dash::valueBox(
                    value = round(stake_2, 2),
                    subtitle = "Stake on Odd 2",
                    icon = shiny::icon("coins"),
                    color = if (stake_2 < 0) "danger" else "success",
                    width = 12
                  )
                ),
                shiny::column(
                  4,
                  bs4Dash::valueBox(
                    value = round(implied_prob_sum, 4),
                    subtitle = "Implied Prob. Sum",
                    icon = shiny::icon("chart-pie"),
                    color = if (implied_prob_sum >= 1) "danger" else "info",
                    width = 12
                  )
                )
              )
            )
          })
        } else {
          output$arbitrage_result <- shiny::renderUI({
            shiny::div(
              class = "result-card result-card-danger",
              shiny::h3(
                shiny::tags$span(
                  class = "label label-danger-custom",
                  shiny::icon("xmark"),
                  " No arbitrage opportunity"
                )
              ),
              shiny::p(
                style = "margin-top: 0.75rem;",
                "With the current odds and stake, there is no arbitrage opportunity."
              )
            )
          })
        }
      }, error = function(e) {
        shinyalert::shinyalert(title = "Error", type = "error",
                               text = e$message, className = 'alert')
      })

    })

  })

}


