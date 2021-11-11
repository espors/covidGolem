
## code to prepare `state_population` dataset goes here


library(readr)

state_population <- read_csv("state_population.csv", 
                             col_types = cols(...1 = col_skip()))


usethis::use_data(state_population, overwrite = TRUE)
