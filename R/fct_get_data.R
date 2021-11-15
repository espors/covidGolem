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
#' @return Reterns the necessary data for the app 
#' @export
#'
#' @examples
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
  
  cumulativeUS <- cumulative_us(covid_states, us_population)
  
  return(list(
    cumulativeUS = cumulativeUS, 
    covid_counties = covid_counties
  ))
}
