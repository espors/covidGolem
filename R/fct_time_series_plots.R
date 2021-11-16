#' time_series_plots 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

NULL 

time_series_plot <- function(covid_data, outcome, pop_level){
  if (pop_level == "states") {
    if (outcome == 1) return(
      
      plotly::ggplotly(ggplot2::ggplot(data = covid_data, 
                                       ggplot2::aes(x = date)) + 
                         ggplot2::geom_line(ggplot2::aes(y = cases, color = state.x), size = 0.75) + 
                         ggplot2::labs(title = tags$h3("Cumulative cases per 100,000 by selected state(s)"), color = "State") + 
                         ggplot2::xlab("Date") + 
                         ggplot2::ylab("Cumulative cases per 100,000") + 
                         ggplot2::theme_minimal()
                       )
      
    )
    if (outcome == 0) return(
      
      plotly::ggplotly(ggplot2::ggplot(data = covid_data, 
                                       ggplot2::aes(x = date)) + 
                         ggplot2::geom_line(ggplot2::aes(y = deaths, color = state.x), size = 0.75) + 
                         ggplot2::labs(title = tags$h3("Cumulative deaths per 100,000 by selected state(s)"), color = "State") + 
                         ggplot2::xlab("Date") + 
                         ggplot2::ylab("Cumulative deaths per 100,000") + 
                         ggplot2::theme_minimal()
      )
    )
  }
}
