#' Prediction UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_predict_ui <- function(id){
  ns <- NS(id)
  tabPanel(
    "Prediction", 
    fluidRow(
      column(
        4, 
        sliderInput(
          inputId = ns("days_forecast"),
          label = "Days to forecast",
          min = 1, 
          max = 14,
          value = 7
        ), 
        sliderInput(
          inputId = ns("days_range"),
          label = "Recent data (days)",
          min = 30, 
          max = 90,
          value = 30
        )
      ), 
      column(
        8, 
        plotOutput(
          outputId = ns("predict_cases_plot")
        )
      )
    )
  )
}
    
#' Predict Server Functions
#'
#' @noRd 
mod_predict_server <- function(id, app_data, states, tab){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$predict_cases_plot <- renderPlot({

      forecast_covid( 
        # Get new cases for selected state
        # Sometimes multiple states are selected, 
        # therefore states$selected_state()[1] for the first
        covid_state = new_cases_state(
          state_covid = app_data$covid_states, 
          state = states$selected_state()[1]
        ), 
        days_range = input$days_range,
        days_forecast = input$days_forecast,
        cases_deaths = states$cases_deaths()
      )
    })
  })
}
    
## To be copied in the UI
# mod_methodology_ui("methodology_ui_1")
    
## To be copied in the server
# mod_methodology_server("methodology_ui_1")
