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
  covid_counties_original <- temp_county
  covid_states_original <- temp_states
  covid_states_original <- covid_states_original %>%
    dplyr::filter(
      state != 'American Samoa' & 
      state != 'Northern Mariana Islands' & 
      state != 'Virgin Islands' & 
      state != 'Puerto Rico' & 
      state != 'Guam'
    )
  # covid_US <- us_rates(
  #   covid_states_original, 
  #   us_population
  # )
  cumulative_US <- cumulative_us(
    covid_states_original, 
    us_population
  )
  covid_counties <- covid_rates(
    covid_counties_original,
    county_population
  )
  cumulative_counties <- cumulative_county(
    covid_counties_original, 
    county_population
  )
  covid_states <- covid_rates(
    covid_states_original, 
    state_population
  )
  cumulative_states <- cumulative_state(
    covid_states_original, 
    state_population
  )
  sir_counties <- sir_counties(
    cumulative_counties, 
    cumulative_states
  )
  sir_states <- sir_states(
    cumulative_states,
    cumulative_US
  )
  state_vaccinations <- vaccinations[-c(1), ]
  us_vaccinations <- vaccinations[1, ]
  return(
    list(
      cumulative_US = cumulative_US, 
      covid_counties = covid_counties, 
      cumulative_counties = cumulative_counties, 
      covid_states = covid_states, 
      cumulative_states = cumulative_states,
      sir_counties = sir_counties, 
      sir_states = sir_states, 
      state_vaccinations = state_vaccinations, 
      us_vaccinations = us_vaccinations
    )
  )
}
