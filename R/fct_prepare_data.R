library(tidyr)
#' prepare_data 
#'
#' @description Prepare covid data 
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' 

NULL 



#' covid_rates
#'
#' @param covid_data 
#' @param population 
#'
#' @return covid_rates
#' @export
#'
#' @examples
#' 
#' 
#' 
#' 
covid_rates <- function(covid_data, population){
  covid_data <- covid_data %>%
    dplyr::filter(state != 'American Somoa')
  earliest <- min(covid_data$date)
  latest <- max(covid_data$date)
  dates <- as.data.frame(lubridate::as_date(earliest:latest))
  dates <- dates %>%
    dplyr::rename(date = `as_date(earliest:latest)`)
  date_matrix <- crossing(population, dates)
  covid_matrix <- dplyr::left_join(date_matrix, covid_data, by = c("fips", "date"))
  covid_matrix[, c("cases", "deaths")] <- na_replace(covid_matrix[,c("cases", "deaths")],0)
  
  
  covid_matrix$cases = round(covid_matrix$cases/covid_matrix$population * 100000,2)
  covid_matrix$deaths = round(covid_matrix$deaths/covid_matrix$population * 100000,2)
  
  return(covid_matrix)
}



#' US Rates
#'
#' @param state_covid 
#' @param us_population 
#'
#' @return US COVID-19 rates in matrix 
#' @export
#'
#' @examples
#' 
#' 

us_rates <- function(state_covid, us_population){
  earliest <- min(state_covid$date)
  latest <- max(state_covid$date)
  dates <- as.data.frame(lubridate::as_date(earliest:latest))
  dates <- dates %>%
    dplyr::rename(date = `lubridate::as_date(earliest:latest)`)
  date_matrix <- crossing(us_population, dates)
  us_covid <- state_covid %>%
    dplyr::group_by(date) %>%
    plyr::summarise(cases = sum(cases), 
              deaths = sum(cases))
  
  covid_matrix <- dplyr::left_join(date_matrix, us_covid, by = c("date"))
  covid_matrix[,c("cases", "deaths")] <- na_replace(covid_matrix[,c("cases", "deaths")], 0)
  
  covid_matrix$cases = round(covid_matrix$cases/covid_matrix$population * 100000,2)
  
  covid_matrix$deaths = round(covid_matrix$deaths/covid_matrix$population * 100000,2)
  
  return(covid_matrix)
}


#' Max Cumulative County Rate
#'
#' @param county_covid 
#' @param county_pop 
#'
#' @return
#' @export
#'
#' @examples
cumulative_county <- function(county_covid, county_pop) {
  covid <- county_covid %>%
    dplyr::filter(date == max(date))
  
  covid_matrix <- dplyr::left_join(covid, county_pop, by = "fips")
  
  covid_matrix$cases = round(covid_matrix$cases/covid_matrix$population * 100000,2)
  covid_matrix$deaths = round(covid_matrix$deaths/covid_matrix$population * 100000,2)
  
  covid_matrix <- tidyr::drop_na(covid_matrix)
  
  return(covid_matrix)
  
}

#' Max Cumulative State Rate 
#'
#' @param state_covid 
#' @param state_pop 
#'
#' @return
#' @export
#'
#' @examples
cumulative_state <- function(state_covid, state_pop) {
  covid <- state_covid %>% 
    dplyr::filter(date == max(date))
  
  covid_matrix <- dplyr::left_join(covid, state_pop, by = "fips")
  
  covid_matrix$cases = round(covid_matrix$cases/covid_matrix$population * 100000,2)
  covid_matrix$deaths = round(covid_matrix$deaths/covid_matrix$population * 100000,2)
  return(covid_matrix)
}


#' Max Cumulative US Rate
#'
#' @param state_covid 
#' @param us_pop 
#'
#' @return
#' @export
#'
#' @examples
cumulative_us <- function(state_covid, us_pop) {
  covid <- state_covid %>%
    dplyr::filter(date == max(date)) %>%
    plyr::summarise(cases = sum(cases), 
              deaths = sum(deaths))
  
  covid_matrix <- crossing(covid, us_pop)
  
  covid_matrix$caseRate <- round(covid_matrix$cases/covid_matrix$population * 100000,2)
  covid_matrix$deathRate <- round(covid_matrix$deaths/covid_matrix$population * 100000,2)
  
  return(covid_matrix)
}

#' SIR Values for Counties
#'
#' @param cumu_county 
#' @param cumu_state 
#'
#' @return
#' @export
#'
#' @examples
sir_counties <- function(cumu_county, cumu_state){
  sir_df <- inner_join(cumu_county, cumu_state, by = 'state.x')
  
  a = 0.05 
  m = length(cumu_county$cases)
  
  sir_df$sir <- round(sir_df$cases.x/sir_df$cases.y,2)
  sir_df$lsir <- round((qchisq(a/(2*m), df = (2*sir_df$cases.x)))/(2*sir_df$cases.y),2)
  sir_df$usir <- round((qchisq(a/(2*m), df = (2*sir_df$cases.x),lower.tail = FALSE))/(2*sir_df$cases.y),2)
  
  sir_df$sdr <- round(sir_df$deaths.x/sir_df$deaths.y,2)
  sir_df$lsdr <- round((qchisq(a/(2*m), df = (2*sir_df$deaths.x)))/(2*sir_df$deaths.y),2)
  sir_df$usdr <- round((qchisq(a/(2*m), df = (2*sir_df$deaths.x),lower.tail = FALSE))/(2*sir_df$deaths.y),2)
  
  sir_df <- sir_df %>%
    dplyr::mutate(typeC = dplyr::case_when(sir > 1 ~ "More", 
                             sir <= 1 ~ "Less"), 
           typeD = dplyr::case_when(sdr > 1 ~ "More", 
                             sdr <= 1 ~ "Less"), 
           significantC = dplyr::case_when(usir > 1 & lsir < 1 ~ 0, 
                                    TRUE ~ 1),
           significantD = dplyr::case_when(usdr > 1 & lsir < 1 ~ 0, 
                                    TRUE ~ 1))
  return(sir_df)
}


#' SIR Values for States
#'
#' @param cumu_state 
#' @param cumu_us 
#'
#' @return
#' @export
#'
#' @examples
sir_states <- function(cumu_state, cumu_us){
  
  cumu_us <- cumu_us %>%
    dplyr::select(caseRate, deathRate)
  
  sir_df <- crossing(cumu_state, cumu_us)
  
  sir_df <- tidyr::drop_na(sir_df)
  
  a = 0.05 
  m = length(cumu_state$cases)
  
  sir_df$sir <- round(sir_df$cases/sir_df$caseRate,2)
  sir_df$lsir <- round((qchisq(a/(2*m), df = (2*sir_df$cases)))/(2*sir_df$caseRate),2)
  sir_df$usir <- round((qchisq(a/(2*m), df = (2*sir_df$cases),lower.tail = FALSE))/(2*sir_df$caseRate),2)
  
  sir_df$sdr <- round(sir_df$deaths/sir_df$deathRate,2)
  sir_df$lsdr <- round((qchisq(a/(2*m), df = (2*sir_df$deaths)))/(2*sir_df$deathRate),2)
  sir_df$usdr <- round((qchisq(a/(2*m), df = (2*sir_df$deaths),lower.tail = FALSE))/(2*sir_df$deathRate),2)
  
  sir_df <- sir_df %>%
    mutate(typeC = dplyr::case_when(sir > 1 ~ "More",
                             sir <= 1 ~ "Less"),
           typeD = dplyr::case_when(sdr > 1 ~ "More",
                             sdr <= 1 ~ "Less"),
           significantC = dplyr::case_when(usir > 1 & lsir < 1 ~ 0,
                                    TRUE ~ 1),
           significantD = dplyr::case_when(usdr > 1 & lsir < 1 ~ 0,
                                    TRUE ~ 1))
  return(sir_df)
}


