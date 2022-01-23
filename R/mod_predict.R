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
        h2("Methodology", align = "center"), 
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

      if(is.null(input$days_range) | 
        is.null(input$days_forecast)) { 
          return(NULL)
      }

      # Get new cases for selected state
      # Sometimes multiple states are selected, 
      # therefore states$selected_state()[1] for the first
      covid_state = new_cases_state(
        state_covid = app_data$covid_states, 
        state = states$selected_state()[1]
      )
      
      # number of time points
      np <- nrow(covid_state) 
      if(np > input$days_range){
        covid_state <- covid_state[(np - input$days_range + 1):np, ]
      }
      
      #switch between cases and deaths
      if( states$cases_deaths() == 1) {#cases
        selected_column <- covid_state$cases
        selected_label <- "Cases"
      } else {
        selected_column <- covid_state$deaths
        selected_label <- "Deaths"
      }

      # Construct timeseries object
      covid_state_ts <- ts(
        data = selected_column, 
        start = c( 
          lubridate::year(min(covid_state$date)), 
          lubridate::yday(min(covid_state$date))
        ), 
        frequency = 365
      )

      # Perform forecast
      forecasted <- forecast::forecast(
        forecast::ets(
          covid_state_ts,
          model = "AAN",
          damped = FALSE
        ),
        input$days_forecast
      )
      
      #Plot
      plot(forecasted,
          xaxt = "n",
          main = "",
          ylab = selected_label,
          xlab = paste0(
            states$selected_state()[1],
            " is expected to have ",
            round(
              forecasted$mean[input$days_forecast],
              2
            )
          )
      )

      # generate a sequence of dates for labelling
      day_seq = seq(
        as.Date(min(covid_state$date)), 
        by="days", 
        length = input$days_forecast + nrow(covid_state) - 1)

      # add date labels
      axis(
        1, 
        at = lubridate::decimal_date(day_seq), 
        labels = format(day_seq, "%b %d")
      )
    })
  })
}
    
## To be copied in the UI
# mod_methodology_ui("methodology_ui_1")
    
## To be copied in the server
# mod_methodology_server("methodology_ui_1")
