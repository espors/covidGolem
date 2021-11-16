#' get_data 
#'
#' @description Get the data 
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

NULL 

#' Get Data 
#'
#' @return Returns the necessary data for the app 
#' @export
#'

get_my_data <- function() {
  
  
  #covid_counties <- readr::read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
  #covid_states <- readr::read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
  
  
  covid_counties <- temp_county
  covid_states <- temp_states
 
  
  covid_states <- covid_states %>%
    dplyr::filter(state != 'American Samoa' & 
                    state != 'Northern Mariana Islands' & 
                    state != 'Virgin Islands' & 
                    state != 'Puerto Rico' & 
                    state != 'Guam')
  
  covid_US <- us_rates(covid_states, us_population)
  
  cumulative_US <- cumulative_us(covid_states, us_population)
  
  covid_counties <- covid_rates(covid_counties, county_population)
  
  cumulative_counties <- cumulative_county(covid_counties, county_population)
  
  covid_states <- covid_rates(covid_states, state_population)
  
  cumulative_states <- cumulative_state(covid_states, state_population)
  
  sir_counties <- sir_counties(cumulative_counties, cumulative_states)
  
  sir_states <- sir_states(cumulative_state, cumulative_US)
  
  return(list(
    covid_US = covid_US, 
    cumulative_US = cumulative_US, 
    covid_counties = covid_counties, 
    cumulative_counties = cumulative_counties, 
    covid_states = covid_states, 
    cumulative_states = cumulative_states,
    sir_counties = sir_counties, 
    sir_states = sir_states
  ))
}
