## code to prepare `temp_states` dataset goes here

library(readr)
temp_states <- read_csv("C:/Users/espor/Box/GRA/COVID19_ShinyApp/Covid19_ShinyApp/COVID-19/covid_states.csv")
temp_states <- readr::read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

usethis::use_data(temp_states, overwrite = TRUE)
vi