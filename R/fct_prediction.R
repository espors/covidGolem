#' Predict future cases and 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

NULL 


#' Forecasting future cases and deaths 
#'
#' @param covid_state   covid data for a state, time series
#' @param days_range    number of days used in forecast
#' @param days_forecast  days to forecase
#' @param cases_deaths   new cases or new deaths
#'
#' @return     plot
#' @export
#'
#' 

forecast_covid <- function(
  covid_state,
  days_range,
  days_forecast,
  cases_deaths                  
  ){

     np <- nrow(covid_state) 
     if(np > days_range){
        covid_state <- covid_state[(np - days_range + 1):np, ]
      }
      
      #switch between cases and deaths
      if( cases_deaths == 1) {#cases
        selected_column <- covid_state$cases
        selected_label <- "New Cases"
      } else {
        selected_column <- covid_state$deaths
        selected_label <- "New Deaths"
      }

      # Construct timeseries object
      covid_state_ts <- ts(
        data = selected_column, 
        start = c( 
          lubridate::year(min(covid_state$date)), 
          lubridate::yday(min(covid_state$date))
        ), 
        frequency = 365
      )

      # Perform forecast
      forecasted <- forecast::forecast(
        forecast::ets(
          covid_state_ts,
          model = "AAN",
          damped = FALSE
        ),
        days_forecast
      )
      
      #Plot
      plot(forecasted,
          xaxt = "n",
          main = "",
          ylab = selected_label,
          xlab = paste0(
            covid_state$state.x[1],
            " is expected to have ",
            round(
              forecasted$mean[days_forecast],
              2
            )
          )
      )

      # generate a sequence of dates for labelling
      day_seq = seq(
        as.Date(min(covid_state$date)), 
        by="days", 
        length = days_forecast + nrow(covid_state) - 1)

      # add date labels
      axis(
        1, 
        at = lubridate::decimal_date(day_seq), 
        labels = format(day_seq, "%b %d")
      )

}