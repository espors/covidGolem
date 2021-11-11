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
    "Counties"
  )
}
    
#' counties Server Functions
#'
#' @noRd 
mod_counties_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_counties_ui("counties_ui_1")
    
## To be copied in the server
# mod_counties_server("counties_ui_1")
