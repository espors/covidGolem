#' united_states UI Function
#'
#' @description United states panel. 
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_united_states_ui <- function(id){
  ns <- shiny::NS(id)
  
  includeCSS("inst/app/www/custom.css")
  
  tabPanel(
    "United States", 
    fluidRow(
      tags$h3("Maps are worth a thousand words..."), 
      column(6, 
             h5("Cumulative Cases per 100,000 in the United States", align = 'center'))
    )
  )
}
    
#' united_states Server Functions
#'
#' @noRd 
mod_united_states_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
