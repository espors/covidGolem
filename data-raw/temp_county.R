## code to prepare `temp_county` dataset goes here

library(readr)
temp_county <- read_csv("C:/Users/espor/Box/GRA/COVID19_ShinyApp/Covid19_ShinyApp/COVID-19/covid_counties.csv")

temp_county <- readr::read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

usethis::use_data(temp_county, overwrite = TRUE)
