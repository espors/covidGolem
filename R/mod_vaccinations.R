#' vaccinations UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_vaccinations_ui <- function(id){
  ns <- NS(id)
  tabPanel("Vaccinations", 
           fluidRow(
             tags$h4("Compare vaccination numbers up to 2021-11-30"), 
             column(
               4, 
               selectizeInput(
                 inputId = ns("map_1"), 
                 label = tags$h5("Select vaccination numbers"), 
                 choices = c(
                   "Doses Delivered per 100K" = 1,
                   "Doses Administered per 100K" = 2, 
                   "At least One Shot per 100K (All Types)" = 3, 
                   "Fully Vaccinated per 100K (All Types)" = 4, 
                   "Fully Vaccinated per 100K (Moderna)" = 5, 
                   "Fully Vaccinated per 100K (Pfizer)" = 6, 
                   "Fully Vaccinated per 100K (Janssen)" = 7, 
                   "Fully Vaccinated per 100K (Other)" = 8
                 ), 
                 selected = "Doses Delivered per 100K"
               ), 
               plotly::plotlyOutput(
                 outputId = ns("map_vaccinations1")
               )
             ), 
             column(
               4, 
               selectizeInput(
                 inputId = ns("map_2"), 
                 label = tags$h5("Select vaccination numbers"), 
                 choices = c(
                   "Doses Delivered per 100K" = 1,
                   "Doses Administered per 100K" = 2, 
                   "At least One Shot per 100K (All Types)" = 3, 
                   "Fully Vaccinated per 100K (All Types)" = 4, 
                   "Fully Vaccinated per 100K (Moderna)" = 5, 
                   "Fully Vaccinated per 100K (Pfizer)" = 6, 
                   "Fully Vaccinated per 100K (Janssen)" = 7, 
                   "Fully Vaccinated per 100K (Other)" = 8
                 ), 
                 selected = "Doses Administered per 100K"
               ), 
               plotly::plotlyOutput(
                 outputId = ns("map_vaccinations2")
               )
             ),
             column(
               4,
               "Difference Between Plots",
               br(),
               br(),
               br(),
               br(),
               br(),
               plotly::plotlyOutput(
                 outputId = ns("map_vaccinations3")
               ),
             )
           )
        )
}
    
#' vaccinations Server Functions
#'
#' @noRd 
mod_vaccinations_server <- function(id, app_data, tab){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    #-----------Maps---------------------------------
    map_data <- map_data_states(
      cumulative_states = app_data$state_vaccinations, 
      method = "vaccinations"
    )
    output$map_vaccinations1 <- plotly::renderPlotly(
      plotly::ggplotly(
        map_united_states(
          states_data = map_data$states_data, 
          map_state = map_data$map_state, 
          method = input$map_1
        )
      )
    )
    output$map_vaccinations2 <- plotly::renderPlotly(
      plotly::ggplotly(
        map_united_states(
          states_data = map_data$states_data, 
          map_state = map_data$map_state, 
          method = input$map_2
        )
      )
    )
    output$map_vaccinations3 <- plotly::renderPlotly(
      plotly::ggplotly(
        basic_map_united_states(
          states_data = map_data$states_data,
          map_1_choice = input$map_1,
          map_2_choice = input$map_2,
          map_state = map_data$map_state
        )
      )
    )
  })
}
    
## To be copied in the UI
# mod_vaccinations_ui("vaccinations_ui_1")
    
## To be copied in the server
# mod_vaccinations_server("vaccinations_ui_1")
