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
                    # selectInput(inputId = "state", 
                    #             label = tags$h5("Select State(s)"), 
                    #             choices = app_data$cumulative_states$state.x, 
                    #             multiple = TRUE, 
                    #             selected = "South Dakota"), 
                    
                    selectizeInput(
                      inputId = ns("state"),
                      label = tags$h5("Select State(s)"), 
                      choices = NULL, 
                      multiple = TRUE
                    ),
                    selectInput(inputId = "cases_deaths", 
                                label = tags$h5("Select Outcome"), 
                                choices = c("Cases" = 1, "Deaths" = 0), 
                                selected = "Cases")
                    ), 
             column(9, 
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
 
  })
}
    
## To be copied in the UI
# mod_states_ui("states_ui_1")
    
## To be copied in the server
# mod_states_server("states_ui_1")
