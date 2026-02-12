box::use(bslib)
box::use(shiny)

#' Numeric input with left-aligned label
#'
#' Reusable UI component that renders a numeric input where the label
#' is displayed to the left of the control instead of above it.
#'
#' @param id Module instance ID used to namespace the input.
#' @param label Character label shown to the left of the input.
#' @param value Initial numeric value (defaults to \code{NA}).
#' @param ... Additional arguments passed to \code{shiny::numericInput}
#'   (for example \code{min}, \code{max}, \code{step}).
#'
#' @return A Shiny UI element that can be included inside layouts.
#'
#' @export
numeric_input_ui <- function(id, label, value = NA, ...) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "input-left-label",
    shiny::span(label),
    shiny::numericInput(
      inputId = ns("value"),
      label   = NULL,
      value   = value,
      min = 0,
      ...
    )
  )
}

#' Server logic for \code{numeric_input_ui}
#'
#' Shiny module server that exposes the numeric value entered in
#' \code{numeric_input_ui}, enforcing a minimum of zero (negative
#' values are reset back to 0).
#'
#' @param id Module instance ID used to namespace the input.
#'
#' @return A reactive expression yielding the current numeric value.
#'
#' @export
numeric_input_server <- function(id) {
  
  shiny::moduleServer(id, function(input, output, session) {

    
    shiny::observeEvent(input$value, {
      if (!is.na(input$value) && input$value < 0) {
        shiny::updateNumericInput(
          session,
          "value",
          value = 0
        )
      }
    }, ignoreInit = TRUE)
    
    shiny::reactive(input$value)
  })
}

