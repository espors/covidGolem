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
             )
 
}
    
#' states Server Functions
#'
#' @noRd 
mod_states_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_states_ui("states_ui_1")
    
## To be copied in the server
# mod_states_server("states_ui_1")
