box::use(bslib)
box::use(shiny)

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

