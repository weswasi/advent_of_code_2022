# December 03 - Advent of Code 2022 ---------------------------------------

# Initiate
library(tidyr)
library(dplyr)
library(stringr)

# Part 1 ------------------------------------------------------------------

# Read data
strategy_data <- read.delim("data/03_dec_data.txt")

# Split each row into two variables by half
for (i in 1:300) {
  strategy_data$first[i] <- substr(strategy_data$items[i], 1, nchar(strategy_data$items[i])/2)
  strategy_data$second[i] <- substr(strategy_data$items[i], nchar(strategy_data$items[i])/2+1, nchar(strategy_data$items[i]))
}
rm(i)

for (i in 1:26) {
  letter_lower <- letters[i]
  letter_upper <- LETTERS[i]
  
  strategy_data <- strategy_data %>% 
    mutate(!!paste0("first_", letter_lower) := 0,
           !!paste0("first_", letter_upper) := 0,
           !!paste0("second_", letter_lower) := 0,
           !!paste0("second_", letter_upper) := 0)
}

for (i in 1:300) {
  for (j in 1:1) { 
    letter_lower <- letters[j]
    letter_upper <- LETTERS[j]
    
    assign(paste0("strategy_data$first_", letter_lower, "[", i, "]"), grepl(paste0(letter_lower), strategy_data$first[i]))
    assign(paste0("strategy_data$first_", letter_upper, "[", i, "]"), grepl(paste0(letter_upper), strategy_data$first[i]))
    
    assign(paste0("strategy_data$first_", letter_lower, "[", i, "]"), paste0("`strategy_data$first_", letter_lower, "[", i, "]`"))
    assign(paste0("strategy_data$first_", letter_upper, "[", i, "]"),  paste0("`strategy_data$first_", letter_upper, "[", i, "]`"))
  }
}

for (i in 1:300) {
  
  d <- data.frame(str_split_fixed(strategy_data$first[i], "", max(nchar(strategy_data$first[i]))),
                  str_split_fixed(strategy_data$second[i], "", max(nchar(strategy_data$second[i]))))
}