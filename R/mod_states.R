#' states UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_states_ui <- function(id){
  ns <- NS(id)
  
  tabPanel("States", 
           fluidRow(
             column(3, 
                    
                    selectizeInput(
                      inputId = ns("state"),
                      label = tags$h5("Select State(s)"), 
                      choices = NULL, 
                      multiple = TRUE
                    ),
                    selectInput(inputId = ns("cases_deaths"), 
                                label = tags$h5("Select Outcome"), 
                                choices = c("Cases" = 1, "Deaths" = 0), 
                                selected = "Cases"), 
                    
                    actionButton(inputId = ns("help_button"), tags$h5("What are SIR values?"))
                    ), 
             column(9, 
                    
                    plotly::plotlyOutput(
                      outputId = ns("map_ts_states")
                    )
                    
                    )
           ), 
           fluidRow(
             
             plotly::plotlyOutput(
               outputId = ns("plot_sir_states")
             )
           )
             )
 
}
    
#' states Server Functions
#'
#' @noRd 
mod_states_server <- function(id, app_data, tab){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    #---- state dropdown list-------------
    observe({
      updateSelectizeInput(
        session = session, 
        inputId = "state", 
        choices = app_data$cumulative_states$state.x,
        selected = "South Dakota", 
        server = TRUE
      )
    })
    
    #--- filter on states-------------
    
    covid_states_input <- reactive({
      app_data$covid_states %>%
        dplyr::filter(state.x %in% input$state)
    })
    
    #---time series plot--------------
    
    output$map_ts_states <- plotly::renderPlotly({
      dataplots = time_series_plot(covid_data = covid_states_input(), 
                                   outcome = input$cases_deaths,
                                   pop_level = "states")
      print(dataplots)
    })
    
    #---SIR information popout------
    
   observeEvent(input$help_button, {
     showModal(modalDialog(
       title = tags$h2("What are SIR values?"), 
       tags$p("Standardized incidence ratio values, or SIR values, 
              compare the observed value in a given population to 
              the expected value based on a reference population. 
              For these plots, the observed value was the given 
              cumulative rate per 100,000 up to the lastest date 
              for each state. The expected value was the cumulative 
              rate per 100,000 up to the lastest date for the United States 
              overall. To caculate each states SIR value, we took one minus 
              the state's value divided by the United State's value. 
              The state's with SIR values close to zero, have a rate 
              that is close to expected, state's with positive values
              have a higher rate than expected, and state's with 
              negative values have a lower rate than expected. 
              Confidence intervals are also featured on the graph. 
              If the interval contains zero, that state has a value 
              that is not siginificantly differeenct from expected."), 
       easyClose = TRUE
     ))
   })
    
    #---sir plot----------------------
    output$plot_sir_states <- plotly::renderPlotly({
      dataplots = sir_plot(sir_data = app_data$sir_states, 
                           outcome = input$cases_deaths, 
                           pop_level = "states")
      
      print(dataplots)
    })
 
  })
}
    
## To be copied in the UI
# mod_states_ui("states_ui_1")
    
## To be copied in the server
# mod_states_server("states_ui_1")
