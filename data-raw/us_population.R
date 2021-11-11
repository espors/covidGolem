## code to prepare `us_population` dataset goes here


library(readr)
us_population <- read_csv("united_states_pop.csv", 
                              col_types = cols(...1 = col_skip()))

usethis::use_data(us_population, overwrite = TRUE)
