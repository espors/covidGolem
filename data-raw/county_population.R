## code to prepare `county_population` dataset goes here

library(readr)

county_population <- read_csv("county_population.csv", 
                              col_types = cols(...1 = col_skip()))

usethis::use_data(county_population, overwrite = TRUE)
