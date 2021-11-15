## code to prepare `temp_states` dataset goes here

library(readr)
temp_states <- read_csv("C:/Users/espor/Box/GRA/COVID19_ShinyApp/Covid19_ShinyApp/COVID-19/covid_states.csv")


usethis::use_data(temp_states, overwrite = TRUE)
