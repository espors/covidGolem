#' counties UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_counties_ui <- function(id){
  ns <- NS(id)
  
  tabPanel(
    "Counties", 
    fluidRow(
      column(3, 
             selectizeInput(
               inputId = ns("state_4_counties"), 
               label = tags$h5("Select State"), 
               choices = NULL, 
               multiple = FALSE, 
             ), 
             uiOutput(
               outputId = ns("counties")
             ),
             selectInput(inputId = ns("cases_deaths"), 
                         label = tags$h5("Select Outcome"), 
                         choices = c("Cases" = 1, "Deaths" = 0), 
                         selected = "Cases")
             ), 
      column(9, 
             plotly::plotlyOutput(
               outputId = ns("map_ts_counties")
             )
             )
    ), 
  
    fluidRow(
      plotly::plotlyOutput(
        outputId = ns("plot_sir_counties")
      )
    )
    
  )
}
    
#' counties Server Functions
#'
#' @noRd 
mod_counties_server <- function(id, app_data, tab){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    #----------state dropdown list----------------
    observe({
      updateSelectizeInput(
        session = session, 
        inputId = "state_4_counties", 
        choices = app_data$cumulative_states$state.x,
        selected = "South Dakota", 
        server = TRUE
      )
    })
    
    #------ county dropdown list -----------------
    
    output$counties <- renderUI({
      selectInput(inputId = ns("choose_counties"), 
                  label = "Select Counties", 
                  choices = app_data$cumulative_counties[app_data$cumulative_counties$state.x == input$state_4_counties, 
                                                         "county.y"], 
                  multiple = TRUE, 
                  selected = "Brookings County")
    })
    
    
    #------- filter on state and counties 
    
    selected_counties <- reactive({
      app_data$covid_counties %>%
        dplyr::filter(state.x %in% input$state_4_counties) %>%
        dplyr::filter(county.x %in% input$choose_counties)
    })
    
    #------- time series plot ------------------
    
    output$map_ts_counties <- plotly::renderPlotly({
      dataplots = time_series_plot(covid_data = selected_counties(), 
                                   outcome = input$cases_deaths, 
                                   pop_level = "counties")
      
      print(dataplots)
    })
    
    #--------sir plot-----------------------
    
    output$plot_sir_counties <- plotly::renderPlotly({
      dataplots = sir_plot(sir_data = app_data$sir_counties, 
                           outcome = input$cases_deaths, 
                           pop_level = "counties")
    })
 
  })
}
    
## To be copied in the UI
# mod_counties_ui("counties_ui_1")
    
## To be copied in the server
# mod_counties_server("counties_ui_1")
