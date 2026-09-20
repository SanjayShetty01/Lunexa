box::use(./components/mod_numeric_input)
box::use(htmltools)
box::use(shinyWidgets)
box::use(../functions/post_bet_arbitrage_calculation)

#' UI for the post-bet hedge calculator module
#'
#' Builds the input form and result area for post-bet hedging, including
#' the original wager, odds, desired profit mode (breakeven, fixed amount,
#' percentage) and the calculate button.
#'
#' @param id Character string used as the Shiny module namespace.
#'
#' @return A Shiny UI element (to be included inside a page layout).
#'
#' @export
post_bet_hedge_page_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(

    shiny::br(),

    # ── Existing Bet Card ─────────────────────────────────
    shiny::fluidRow(
      shiny::column(
        12,
        bs4Dash::box(
          title = shiny::tagList(shiny::icon("receipt"), " Your Existing Bet"),
          status = "primary",
          solidHeader = FALSE,
          collapsible = FALSE,
          width = 12,
          shiny::fluidRow(
            shiny::column(
              6,
              mod_numeric_input$numeric_input_ui(
                id = ns("entered_wage"),
                label = "Wager",
                step = 0.01
              )
            ),
            shiny::column(
              6,
              mod_numeric_input$numeric_input_ui(
                id = ns("entered_odd"),
                label = "Odd",
                step = 0.01
              )
            )
          )
        )
      )
    ),

    # ── Hedge Strategy Card ───────────────────────────────
    shiny::fluidRow(
      shiny::column(
        8, offset = 2,
        bs4Dash::box(
          title = shiny::tagList(shiny::icon("sliders"), " Hedge Strategy"),
          status = "warning",
          solidHeader = FALSE,
          collapsible = FALSE,
          width = 12,
          shiny::selectInput(
            inputId = ns("calculation_type"),
            label = "Profit or Breakeven",
            choices = list(
              "Breakeven" = "breakeven",
              "Guaranteed Profit in Rupees" = "guaranteed_rupees",
              "Guaranteed Profit in %" = "guaranteed_percent"
            ),
            selected = 'breakeven'
          ),

          shiny::conditionalPanel(
            condition = "input.calculation_type === 'breakeven'",
            ns = ns
          ),
          shiny::conditionalPanel(
            condition = "input.calculation_type === 'guaranteed_rupees'",
            mod_numeric_input$numeric_input_ui(
              id = ns("expected_profit_amt"),
              label = "Profit (Amount)",
              step = 1
            ),
            ns = ns
          ),
          shiny::conditionalPanel(
            condition = "input.calculation_type === 'guaranteed_percent'",
            mod_numeric_input$numeric_input_ui(
              id = ns("expected_profit_pct"),
              label = "Profit (%)",
              step = 1
            ),
            ns = ns
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
          inputId = ns("calculate_post_bet_arbitrage"),
          label = "Calculate Hedge",
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
        shiny::uiOutput(ns("post_bet_arbitrage"))
      )
    )
  )
}

#' Server logic for the post-bet hedge calculator module
#'
#' Validates inputs and, based on the selected calculation type, computes
#' the required hedge stake and odds to either breakeven or lock in a chosen
#' profit (absolute or percentage), then renders a formatted summary.
#'
#' @param id Character string used as the Shiny module namespace.
#'
#' @return The server function is called for its side effects
#'   (rendering outputs and registering reactives) and does not
#'   return a value.
#'
#' @export
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

          description <- switch(
            input$calculation_type,
            "breakeven" = "Values needed to fully hedge and break even:",
            "guaranteed_rupees" = "Values needed to lock in the requested profit (amount):",
            "guaranteed_percent" = "Values needed to lock in the requested profit (percentage):",
            "Hedge recommendation:"
          )

          shiny::tagList(
            # Status banner
            shiny::div(
              class = "result-card result-card-info",
              shiny::h3(
                shiny::tags$span(
                  class = "label label-primary-custom",
                  shiny::icon("shield-halved"),
                  " Post-bet hedge recommendation"
                )
              ),
              shiny::p(
                style = "margin-top: 0.5rem; color: #9a3412;",
                description
              )
            ),

            shiny::br(),

            # Value boxes
            shiny::fluidRow(
              shiny::column(
                6,
                bs4Dash::valueBox(
                  value = round(required_odd, 2),
                  subtitle = "Required Hedge Odd",
                  icon = shiny::icon("bullseye"),
                  color = if (required_odd < 0) "danger" else "primary",
                  width = 12
                )
              ),
              shiny::column(
                6,
                bs4Dash::valueBox(
                  value = round(stake_2, 2),
                  subtitle = "Stake on Hedge Bet",
                  icon = shiny::icon("hand-holding-dollar"),
                  color = if (stake_2 < 0) "danger" else "success",
                  width = 12
                )
              )
            )
          )
        })

      }, error = function(e) {
        shinyalert::shinyalert(title = "Error", type = "error",
                               text = e$message, className = 'alert')
      }
      )
    })

  })

}


