#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  
  # tab variable to control reactivity 
  tab <- reactive(input$navbar)
  
  app_data <- get_my_data()
  
  
  mod_united_states_server(
    id = "united_states_ui_1", 
    app_data = app_data,
    tab = tab
    )
  
  states <- mod_states_server(
     id = "states_ui_1", 
     app_data = app_data, 
     tab = tab
     )
  
  counties <- mod_counties_server(
    id = "counties_ui_1" #, 
    #tab = tab 
    )
  
  methodology <- mod_methodology_server(
    id = "methodology_ui_1" #, 
    #tab = tab
    )
}
