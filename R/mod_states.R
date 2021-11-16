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
                    selectInput(inputId = "state", 
                                label = tags$h5("Select State(s)"), 
                                choices = app_data$cumulative_states$state.x, 
                                multiple = TRUE, 
                                selected = "South Dakota"), 
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
 
  })
}
    
## To be copied in the UI
# mod_states_ui("states_ui_1")
    
## To be copied in the server
# mod_states_server("states_ui_1")
