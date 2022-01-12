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
      tags$h4(
        textOutput(
          outputId = ns("ts_date")
        )
      ),
      column(
        3, 
        selectizeInput(
          inputId = ns("state_4_counties"), 
          label = tags$h5("Select State"), 
          choices = NULL, 
          multiple = FALSE, 
        ), 
        uiOutput(
          outputId = ns("counties")
        ),
        selectInput(
          inputId = ns("cases_deaths"), 
          label = tags$h5("Select Outcome"), 
          choices = c("Cases" = 1, "Deaths" = 0), 
          selected = "Cases"
        )
      ), 
      column(
        9, 
        tabsetPanel(
          type = "tabs", 
          tabPanel(
            "Rate",  
            plotly::plotlyOutput(
                 outputId = ns("map_ts_counties")
            )
          ), 
          tabPanel(
            "Log Rate",  
            plotly::plotlyOutput(
                 outputId = ns("map_ts_counties_log")
            )
          )
        )
      )
    ), 
    fluidRow(
      tags$h4(
        textOutput(
          outputId = ns("sir_date")
        )
      ),
      column(
        3, 
        tags$br(), 
        tags$br(), 
        actionButton(
          inputId = ns("help_button"), 
          tags$h5("What are SIR values?")
        ), 
        tags$br(), 
        tags$br(), 
        tags$br(),
        img(
          src = "www/covid19.png", 
          align = "center", 
          width = "200", 
          height = "200"
        )
      ), 
      column(
        9, 
        plotly::plotlyOutput(
          outputId = ns("plot_sir_counties")
        )
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
    #---- Latest Dates -------------------------
    output$ts_date <- renderText({
      paste(
        "COVID-19 outcomes per 100,000 over time from 2020-01-21 to", 
        max(app_data$cumulative_states$date)
      )
    })
    output$sir_date <- renderText({
      paste(
        "   Standarized incidence ratio (SIR) values from 2020-01-21 to", 
        max(app_data$cumulative_states$date)
      )
    })
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
                  label = tags$h5("Select Counties"), 
                  choices = 
                    app_data$cumulative_counties[app_data$cumulative_counties$state.x == input$state_4_counties, 
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
    selected_state <- reactive({
      app_data$sir_counties %>%
        dplyr::filter(state.x %in% input$state_4_counties)
    })
    #------- time series plot ------------------
    output$map_ts_counties <- plotly::renderPlotly({
      dataplots = time_series_plot(covid_data = selected_counties(), 
                                   outcome = input$cases_deaths, 
                                   pop_level = "counties")[[1]]
    })
    output$map_ts_counties_log <- plotly::renderPlotly({
      dataplots = time_series_plot(covid_data = selected_counties(), 
                                   outcome = input$cases_deaths, 
                                   pop_level = "counties")[[2]]
    })
    #---SIR information pop out --------------
    observeEvent(input$help_button, {
      showModal(modalDialog(
        title = tags$h2("What are SIR values?"), 
        tags$p(
        "Standardized incidence ratio values, or SIR values, 
              compare the observed value in a given population to 
              the expected value based on a reference population. 
              For these plots, the observed value was the given 
              cumulative rate per 100,000 up to the lastest date 
              for each county. The expected value was the cumulative 
              rate per 100,000 up to the lastest date for the selected 
              state. To caculate each states SIR value, we took one minus 
              the county's value divided by the states's value. 
              The county's with SIR values close to zero, have a rate 
              that is close to expected, county's with positive values
              have a higher rate than expected, and county's with 
              negative values have a lower rate than expected. 
              Confidence intervals are also featured on the graph. 
              If the interval contains zero, that county has a value 
              that is not siginificantly differeenct from expected."), 
          easyClose = TRUE
        )
      )
    })
    #--------sir plot-----------------------
    output$plot_sir_counties <- plotly::renderPlotly({
      dataplots = sir_plot(sir_data = selected_state(), 
                           outcome = input$cases_deaths, 
                           pop_level = "counties")
    })
  })
}
## To be copied in the UI
# mod_counties_ui("counties_ui_1")
    
## To be copied in the server
# mod_counties_server("counties_ui_1")
