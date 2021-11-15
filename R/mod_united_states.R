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
             h5("Cumulative Cases per 100,000 in the United States", align = 'center')
             )
    ), 
    fluidRow(
      h3("By the numbers..."),
      column(3,
             h5("Cumulative Cases in the US (Raw Counts)", align = 'center'),
             wellPanel(style = "background: #aec3b0",
               h4(textOutput(
                outputId = ns("rawCases")),
                align = 'right')
             )
      ), 
      column(3,
             h5("Cumulative Cases\nPer 100,000 in the US", align = 'center'),
             wellPanel(style = "background: #aec3b0",
                       h4(textOutput(
                         outputId = ns("cumuCases")),
                         align = 'right')
             )
            
      ),
      column(3,
             h5("Cumulative Deaths\n(Raw Counts) in the US", align = 'center'),
             wellPanel(style = "background: #aec3b0",
                       h4(textOutput(
                         outputId = ns("rawDeaths"))
                         , align = 'right')       
             )
             
             
      ), 
      column(3, 
             h5("Cumulative Deaths\nPer 100,000 in the US", align = 'center'),
             wellPanel(style = "background: #aec3b0",
                       h4(textOutput(
                         outputId = ns("cumuDeaths")), 
                         align = 'right')   
             )
            
      )
    ), 
    fluidRow(
      wellPanel(style = "background: #aec3b0",
                h4("For accurate information on the COVID-19 pandemic, please visit the website of your local health department or government resource,
                        such the CDC.", align = "center"),
                tags$a(href = "https://www.cdc.gov/coronavirus/2019-ncov/index.html", tags$h4(tags$u("CDC.gov: COVID-19"), style = "color: blue"), align = "center"),
                h4("Use the following link to find the nearest COVID-19 vaccination provider.", align = "center"),
                tags$a(href = "https://www.vaccines.gov/", tags$h4(tags$u("Vaccines.gov"), style = "color: blue"), align = "center")
      ))
    
  )
}
    
#' united_states Server Functions
#'
#' @noRd 
mod_united_states_server <- function(id, app_data, tab){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    #----------Total numbers for United States-------
    
    output$rawCases <- renderText({app_data$cumulativeUS$cases})
    
    output$cumuCases <- renderText({round(app_data$cumulativeUS$caseRate,0)})
    
    output$rawDeaths <- renderText({app_data$cumulativeUS$deaths})
    
    output$cumuDeaths <- renderText({round(app_data$cumulativeUS$deathRate,0)})
 
  })
}
    
