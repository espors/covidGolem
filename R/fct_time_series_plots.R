#' time_series_plots 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

NULL 

time_series_plot <- function(covid_data, outcome, pop_level){
  validate(
    need((nrow(covid_data) > 0 ), "Please make a selection.")
  )
  if (pop_level == "states") {
    if (outcome == 1) {
     rate <-  plotly::ggplotly(
       ggplot2::ggplot(
         data = covid_data) + 
         ggplot2::geom_line(ggplot2::aes(
           y = cases, 
           x = date, 
           colour = state.x
           )
         ), 
       size = 0.5) + 
       ggplot2::labs(
         title = 
           tags$h3("Daily cumulative cases per 100,000 by selected state(s)"), 
         color = "State"
        ) + 
       ggplot2::xlab("Date") + 
       ggplot2::ylab("Cumulative cases per 100,000") + 
       ggplot2::theme_minimal() 
     rate_log <-  plotly::ggplotly(
       ggplot2::ggplot(
         data = covid_data
        ) +  
         ggplot2::geom_line(
           ggplot2::aes(
             y = cases_log, 
             x = date, 
             colour = state.x 
           ), 
           size = 0.5) + 
         ggplot2::labs(
           title = tags$h3("Daily log cumulative cases by selected state(s)"
           ), 
           color = "State"
         ) + 
         ggplot2::xlab("Date") + 
         ggplot2::ylab("Log cumulative cases") + 
         ggplot2::theme_minimal() 
      )
     return(
       list(
         rate, 
         rate_log
       )
     )
    }
    if (outcome == 0) {
      rate <- plotly::ggplotly(
        ggplot2::ggplot(
          data = covid_data, 
          ggplot2::aes(x = date)
        ) + 
          ggplot2::geom_line(
            ggplot2::aes(
              y = deaths,
              color = state.x
            ), 
            size = 0.75
          ) + 
          ggplot2::labs(
            title = 
              tags$h3(
                "Daily cumulative deaths per 100,000 by selected state(s)"
              ), 
            color = "State"
          ) + 
          ggplot2::xlab("Date") + 
          ggplot2::ylab("Cumulative deaths per 100,000") + 
          ggplot2::theme_minimal()
      )
      rate_log <- plotly::ggplotly(
        ggplot2::ggplot(
          data = covid_data, 
          ggplot2::aes(x = date)
        ) + 
          ggplot2::geom_line(
            ggplot2::aes(
              y = deaths_log, 
              color = state.x), 
            size = 0.75
          ) + 
          ggplot2::labs(
            title = tags$h3("Daily log cumulative deaths by selected state(s)"),
            color = "State"
          ) + 
          ggplot2::xlab("Date") + 
          ggplot2::ylab("Log cumulative deaths") + 
          ggplot2::theme_minimal()
      )
      return(
        list(
          rate, 
          rate_log
        )
      )                            
    }
  }
  if (pop_level == "counties") {
    if (outcome == 1) {
      rate <- plotly::ggplotly(ggplot2::ggplot(
        data = covid_data, 
        ggplot2::aes(x = date)
      ) + 
        ggplot2::geom_line(
          ggplot2::aes(
            y = cases, 
            color = county.x), 
          size = 0.75
        ) + 
        ggplot2::labs(
          title = "Daily cumulative cases per 100,000 by selected counties", 
          color = "County"
        ) + 
        ggplot2::xlab("Date") + 
        ggplot2::ylab("Cumulative cases per 100,000") + 
        ggplot2::theme_minimal()
      )
      rate_log <- plotly::ggplotly(
        ggplot2::ggplot(
          data = covid_data,
          ggplot2::aes(x = date)
        ) + 
          ggplot2::geom_line(
            ggplot2::aes(
              y = cases_log, 
              color = county.x
              ), 
            size = 0.75
          ) + 
          ggplot2::labs(
            title = "Daily log cumulative cases by selected counties", 
            color = "County"
          ) + 
          ggplot2::xlab("Date") + 
          ggplot2::ylab("Log cumulative cases") + 
          ggplot2::theme_minimal()
      )
      return(
        list(
          rate, 
          rate_log
        )
      )    
    }
    if (outcome == 0) {
      rate <- plotly::ggplotly(
        ggplot2::ggplot(
          data = covid_data, 
          ggplot2::aes(x = date)
        ) + 
          ggplot2::geom_line(
            ggplot2::aes(
              y = deaths,
              color = county.x
            ),
            size = 0.75
          ) + 
          ggplot2::labs(
            title = "Daily cumulative deaths per 100,000 by selected counties",
            color = "County"
          ) + 
          ggplot2::xlab("Date") + 
          ggplot2::ylab("Cumulative deaths per 100,000") + 
          ggplot2::theme_minimal()
      )
      rate_log <- plotly::ggplotly(
        ggplot2::ggplot(
          data = covid_data, 
          ggplot2::aes(x = date)
        ) +
          ggplot2::geom_line(
            ggplot2::aes(
              y = deaths, 
              color = county.x
            ), 
            size = 0.75
          ) + 
          ggplot2::labs(
            title = "Daily cumulative deaths per 100,000 by selected counties", 
            color = "County"
          ) + 
          ggplot2::xlab("Date") + 
          ggplot2::ylab("Cumulative deaths per 100,000") + 
          ggplot2::theme_minimal()
      )
      return(
        list(
          rate, 
          rate_log
        )
      )   
    }
  }
}
