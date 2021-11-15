#' methodology UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_methodology_ui <- function(id){
  ns <- NS(id)
 
  tabPanel(
    "Data and Methodology", 
    fluidRow(
      column(6, 
             tags$h2("Data Sources", align = "center"), 
             tags$h4("COVID-19 Data"), 
             tags$p("The COVID-19 data was taken from The New York Times Git-Hub page, https://github.com/nytimes/covid-19-data. 
                               The data included the daily cumulative number of cases and deaths reported in each county and state as reported by local health agencies
                               since the beginning of the pandemic in the United States, January 21, 2020. The counts are updated each day and relect the final numbers as 
                               reported from the previous day. Please see their GitHub page for further information about data reporting."), 
             h4("Population Data"),
             tags$p("The population data for the states and counties 
                    was taken from the United State Census Bureau based on 2019 estimates of the 2010 Census.")
             ), 
      column(6, 
             h2("Methodology", align = "center"), 
             h4("Population Adjustment"), 
             tags$p("The data was prepared using a population adjustment. That is, the cumulative rate 
                    in a particular region was divided by the population of that region which was the 
                    multipled by 100,000. The resulting number was the rate per 100,000."))
    )
  )
}
    
#' methodology Server Functions
#'
#' @noRd 
mod_methodology_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_methodology_ui("methodology_ui_1")
    
## To be copied in the server
# mod_methodology_server("methodology_ui_1")
